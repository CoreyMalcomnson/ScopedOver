using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.Events;

public class Health : NetworkBehaviour
{
    public UnityEvent OnDeath;

    public bool IsDead => healthNetworkVar.Value == 0;

    [SerializeField] private int maxHealth = 100;
    [SerializeField] private float timeBeforeRecovery = 5f;
    [SerializeField] private int healthPerRecovery = 10;
    [SerializeField] private int secondsPerRecovery = 1;

    private NetworkVariable<int> healthNetworkVar = new NetworkVariable<int>(writePerm: NetworkVariableWritePermission.Server);

    private float timeSinceLastDamaged;
    private float recoveryTimer;

    public override void OnNetworkSpawn()
    {
        if (IsServer)
        {
            healthNetworkVar.Value = maxHealth;
        }
    }

    private void Update()
    {
        UpdateRecovery();
    }

    private void UpdateRecovery()
    {
        if (!IsServer) return;
        if (IsDead) return;

        if (Time.time - timeSinceLastDamaged > timeBeforeRecovery && healthNetworkVar.Value < maxHealth)
        {
            recoveryTimer -= Time.deltaTime;
            if (recoveryTimer <= 0)
            {
                int newValue = Mathf.Clamp(healthNetworkVar.Value + healthPerRecovery, 0, maxHealth); ;
                healthNetworkVar.Value = newValue;

                recoveryTimer = secondsPerRecovery;
            }
        }
    }

    public void Damage(NetworkObject sender, int amount)
    {
        // Already dead
        if (IsDead) return;

        timeSinceLastDamaged = Time.time;

        // Find new clamped value
        int newValue = healthNetworkVar.Value - amount;
        if (newValue < 0)
            newValue = 0;

        // Update health
        healthNetworkVar.Value = newValue;

        // Check if dead
        if (IsDead)
        {
            OnDeathClientRPC();
        }
    }

    [ClientRpc]
    private void OnDeathClientRPC()
    {
        OnDeath?.Invoke();
    }

    public int GetMaxHealth()
    {
        return maxHealth;
    }

    public NetworkVariable<int> GetHealthNetworkVar()
    {
        return healthNetworkVar;
    }
}
