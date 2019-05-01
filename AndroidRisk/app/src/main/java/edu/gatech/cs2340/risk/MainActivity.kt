package edu.gatech.cs2340.risk

import android.content.Intent
import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.view.WindowManager
import android.widget.Button

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        this.window.setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN)

        val startGameButton: Button = findViewById(R.id.button)
        startGameButton.setOnClickListener {
            val intent = Intent(this, PlayerLobbyActivity::class.java)
            startActivity(intent)
        }
    }


}
