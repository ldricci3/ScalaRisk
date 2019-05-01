package edu.gatech.cs2340.risk

import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.support.v7.widget.LinearLayoutManager
import android.support.v7.widget.RecyclerView
import android.view.inputmethod.EditorInfo
import android.widget.EditText
import kotlinx.android.synthetic.main.activity_player_lobby.*
import android.content.Context.INPUT_METHOD_SERVICE
import android.content.Intent
import android.support.v4.content.ContextCompat.getSystemService
import android.view.inputmethod.InputMethodManager
import android.support.v4.content.ContextCompat.getSystemService
import android.view.WindowManager
import android.widget.Button
import android.widget.Toast
import java.lang.StringBuilder


class PlayerLobbyActivity : AppCompatActivity() {

    private val playerData: ArrayList<Player> = arrayListOf()
    private val adapter = PlayerLobbyAdapter(playerData)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_player_lobby)
        setUpRecyclerView(recycler_view)

        this.window.setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN)

        val playerAdd = findViewById<EditText>(R.id.playerInput)
        playerAdd.setOnFocusChangeListener { v, hasFocus ->
            if (hasFocus) {
                val imm = getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
                imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0)
            }
        }
        playerAdd.setOnEditorActionListener { view, actionId, _ ->
            if (actionId == EditorInfo.IME_ACTION_DONE) {
                if (!(playerAdd.text.toString() == "")) {
                    playerData.add(Player(playerAdd.text.toString()))
                }
                playerAdd.text.clear()
                setUpRecyclerView(recycler_view)
                val imm = view.context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
                imm.hideSoftInputFromWindow(view.windowToken, 0)
                return@setOnEditorActionListener true
            }
            return@setOnEditorActionListener false
        }

        val resetBtn = findViewById<Button>(R.id.resetBtn)
        resetBtn.setOnClickListener {
            playerData.clear()
            setUpRecyclerView(recycler_view)
        }

        val startBtn = findViewById<Button>(R.id.startBtn)
        startBtn.setOnClickListener {
            initializeGame()
        }

        val joinBtn = findViewById<Button>(R.id.joinBtn)
        joinBtn.setOnClickListener {
            val intent = Intent(this, Game::class.java)
            startActivity(intent)
        }
    }

    private fun setUpRecyclerView(recyclerView: RecyclerView) {
        recyclerView.layoutManager = LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false)
        recyclerView.adapter = adapter
    }

    private fun initializeGame() {
        if (adapter.itemCount < 3 || adapter.itemCount > 6) {
            Toast.makeText(this, "The number of players must be between 3 and 6 inclusive.", Toast.LENGTH_LONG).show()
        } else {
            val intent = Intent(this, Game::class.java)
            var players = ""
            for (player in playerData) {
                players += player.name + ","
            }
            intent.putExtra("PLAYERS", players)
            startActivity(intent)
        }
    }
}