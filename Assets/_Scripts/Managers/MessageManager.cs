﻿using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System.Collections.Generic;
using TMPro;
using UnityEngine.UI;

public class MessageManager : NetworkBehaviour
{
    public static MessageManager Instance;

    public override void OnNetworkSpawn()
    {
        Instance = this;
    }

    [ClientRpc]
    private void ReplicateMessageClientRPC(string message)
    {
        MessageController.AddNewMessage(message);
    }

    [ServerRpc(RequireOwnership = false)]
    public void SendMessageServerRPC(string message, ServerRpcParams serverRpcParams = default)
    {
        if (string.IsNullOrEmpty(message)) return;


        ulong clientId = serverRpcParams.Receive.SenderClientId;
        Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
        if (player == null) return;

        if (CheckForCommand(clientId, message))
        {
            return;
        }

        ReplicateMessageClientRPC($"{player.GetUsername().Value}: {message}");
    }

    private bool CheckForCommand(ulong clientId, string message)
    {
        if (!message.StartsWith("/"))
            return false;

        if (message.Contains("/respawn"))
        {
            Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
            if (player != null)
            {
                player.SpawnCharacter();
            }

            return true;
        }

        if (message.Contains("/tp"))
        {
            string[] args = message.Split(" ");
            if (args.Length >= 2)
            {

                Character character = NetworkObjectManager.Instance.GetCharacter(clientId);
                if (character != null)
                {
                    Character otherCharacter = NetworkObjectManager.Instance.GetCharacter(args[1]);
                    if (otherCharacter != null)
                    {
                        character.TeleportToOtherCharacter(otherCharacter);
                        return true;
                    }
                }
            }
        }

        if (message.Contains("/setname"))
        {
            string[] args = message.Split(" ");
            if (args.Length >= 2)
            {
                Player player = NetworkObjectManager.Instance.GetPlayer(clientId);
                if (player != null)
                {
                    player.SetUsername(args[1]);
                }
            }
        }

        return false;
    }

}