using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System.Collections.Generic;

public class NetworkObjectManager: NetworkBehaviour
{
    public static NetworkObjectManager Instance;

    private Dictionary<ulong, Player> playerDictionary;
    private Dictionary<ulong, Character> characterDictionary;

    private void Awake()
    {
        Instance = this;

        playerDictionary = new();
        characterDictionary = new();
    }

    public void AddPlayer(ulong clientId, Player player)
    {
        playerDictionary[clientId] = player;
    }

    public void AddCharacter(ulong clientId, Character character)
    {
        characterDictionary[clientId] = character;
    }

    public Player GetPlayer(ulong clientId)
    {
        bool hasValue = playerDictionary.TryGetValue(clientId, out Player player);
        return hasValue ? player : null;
    }

    public Character GetCharacter(ulong clientId)
    {
        bool hasValue = characterDictionary.TryGetValue(clientId, out Character character);
        return hasValue ? character : null;
    }

    public void RemovePlayer(ulong clientId)
    {
        playerDictionary.Remove(clientId);
    }

    public void RemoveCharacter(ulong clientId)
    {
        characterDictionary.Remove(clientId);
    }
}
