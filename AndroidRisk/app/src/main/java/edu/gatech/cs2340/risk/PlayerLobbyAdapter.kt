package edu.gatech.cs2340.risk

import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView

class PlayerLobbyAdapter(private val playerList: ArrayList<Player>): RecyclerView.Adapter<CustomViewHolder>() {
    override fun getItemCount(): Int {
        return playerList.size
    }
    override fun onBindViewHolder(holder: CustomViewHolder, position: Int) {
        holder.bindViewItems(playerList[position])
    }
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): CustomViewHolder {
        val layoutInflater = LayoutInflater.from(parent.context)
        val cell = layoutInflater.inflate(R.layout.player_name, parent, false)
        return CustomViewHolder(cell)
    }
}

class CustomViewHolder(private val view: View): RecyclerView.ViewHolder(view) {
    fun bindViewItems(player: Player) {
        view.findViewById<TextView>(R.id.name).text = player.name
    }
}