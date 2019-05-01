package edu.gatech.cs2340.risk

import android.annotation.SuppressLint
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.WindowManager
import android.widget.*
import kotlinx.android.synthetic.main.activity_game.*
import kotlinx.android.synthetic.main.toolbar.*
import org.json.JSONObject
import java.net.URL
import org.jetbrains.anko.*


class Game : AppCompatActivity() {

    private val domain: String = "https://scala-risk.serveo.net/"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_game)

        this.window.setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN)

        val players = intent.getStringExtra("PLAYERS")

        webview.loadUrl(domain + "mobile")
        webSettings()
        async()

        val submitBtn = findViewById<Button>(R.id.submitAction)
        submitBtn.setOnClickListener {
            if (actionToTake.text == "Action To Take: place armies"
                && spinner1.selectedItem.toString() != "Place Army"
                && spinner1.selectedItem.toString() != "Next") {
                Toast.makeText(this, "Incorrect Action", Toast.LENGTH_LONG).show()
            } else if (actionToTake.text == "Action To Take: attack enemy territories"
                && spinner1.selectedItem.toString() != "Attack"
                && spinner1.selectedItem.toString() != "Next") {
                Toast.makeText(this, "Incorrect Action", Toast.LENGTH_LONG).show()
            } else if (actionToTake.text == "Action To Take: fortify your territories"
                && spinner1.selectedItem.toString() != "Fortify"
                && spinner1.selectedItem.toString() != "Next") {
                Toast.makeText(this, "Incorrect Action", Toast.LENGTH_LONG).show()
            } else {
                postRequest()
            }
        }

    }

    @SuppressLint("SetJavaScriptEnabled")
    fun webSettings() {
        val webSettings = webview.settings
        webSettings.javaScriptEnabled = true
        webSettings.domStorageEnabled = true
        webSettings.loadWithOverviewMode = true
        webSettings.useWideViewPort = true
        webSettings.builtInZoomControls = true
        webSettings.displayZoomControls = false
        webSettings.setSupportZoom(true)
        webSettings.defaultTextEncodingName = "utf-8"
    }


    fun async() {
        doAsyncResult {
            val result = URL(domain + "json").readText()
            val jsonObj = JSONObject(result)
            val jsonArr = jsonObj.getJSONArray("Player Territories")
            val strArr = arrayOfNulls<String>(jsonArr.length())
            for (i in 0 until strArr.size) {
                strArr[i] = jsonArr.getString(i)
            }
            uiThread {
                if (jsonObj.getString("Current Action") == "Battling!") {
                    Toast.makeText(applicationContext, "Attacker rolled: " + jsonObj.getString("Attacking Dice")
                            + " and Defender rolled: " + jsonObj.getString("Defending Dice"), Toast.LENGTH_LONG).show()
                }
                currentPlayer.text = applicationContext.getString(R.string.currPlayer, jsonObj.getString("Current Player"), jsonObj.getString("Player Color"))
                actionToTake.text = applicationContext.getString(R.string.action, jsonObj.getString("Current Action"))
                message.text = applicationContext.getString(R.string.message, jsonObj.getString("Submission Message"))

                val spinnerAdapter = ArrayAdapter<String>(applicationContext, R.layout.spinner_item, strArr)
                spinnerAdapter.setDropDownViewResource(R.layout.spinner_item)
                spinner2.adapter = spinnerAdapter

                val spinnerAdapter2 = ArrayAdapter<String>(applicationContext, R.layout.spinner_item, resources.getStringArray(R.array.actions))
                spinnerAdapter2.setDropDownViewResource(R.layout.spinner_item)
                spinner1.adapter = spinnerAdapter2

            }
        }
    }

    fun async2() {
        doAsync {
            val result = URL(domain + "json").readText()
            val jsonObj = JSONObject(result)
            uiThread {
                currentPlayer.text = applicationContext.getString(R.string.currPlayer, jsonObj.getString("Current Player"), jsonObj.getString("Player Color"))
                actionToTake.text = applicationContext.getString(R.string.action, jsonObj.getString("Current Action"))
                message.text = applicationContext.getString(R.string.message, jsonObj.getString("Submission Message"))
            }
        }
    }

    fun postRequest() {
        val territory = spinner2.selectedItem.toString().replace(" ", "_")
        var territoryToAtk: String
        val action = spinner1.selectedItem.toString()
        if (action != "Next" && num.text.toString() == "") {
            Toast.makeText(this, "Enter number of armies.", Toast.LENGTH_LONG).show()
            return
        }
        if (action == "Place Army") {
            webview.loadUrl(domain + "mobile/place(" + territory + ", " + num.text.toString() + ")")
            finish()
            startActivity(intent)
        } else if (action == "Attack") {
            selector("Pick territory to attack.", resources.getStringArray(R.array.countries).asList()) { _, i ->
                territoryToAtk = resources.getStringArray(R.array.countries)[i]
                webview.loadUrl(domain + "mobile/attack(" + territoryToAtk.replace(" ", "_") + ", "
                        + territory + ", " + num.text.toString() + ")")
            }
            finish()
            startActivity(intent)
        } else if (action == "Defend") {
            webview.loadUrl(domain + "mobile/defend(" + num.text.toString() + ")")
            finish()
            startActivity(intent)
        } else if (action == "Fortify") {
            selector("Pick territory from which you want to send troops to.", resources.getStringArray(R.array.countries).asList()) { _, i ->
                webview.loadUrl(domain + "mobile/fortify(" + territory + ", "
                        + resources.getStringArray(R.array.countries)[i].replace(" ", "_") + ", "
                        + num.text.toString() + ")")
            }
            finish()
            startActivity(intent)
        } else if (action == "Next") {
            webview.loadUrl(domain + "mobile/next")
            finish()
            startActivity(intent)
        }
        async()
    }



}
