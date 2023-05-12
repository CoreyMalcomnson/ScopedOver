using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class Scoreboard : NetworkBehaviour
{
    public static Scoreboard Instance;

    private Dictionary<Player, int> killsDictionary;

    public override void OnNetworkSpawn()
    {
        Instance = this;
        killsDictionary = new();
    }

    public void AddKill(Player killer, string killed)
    {
        if (killsDictionary.ContainsKey(killer))
        {
            killsDictionary[killer] += 1;
        } else
        {
            killsDictionary.Add(killer, 1);
        }

        MessageManager.Instance.ReplicateMessageClientRPC($"~{killer.GetUsernameNetworkVar().Value.ToString()} just killed {killed}! Total kills {killsDictionary[killer]}.");
    }
}
