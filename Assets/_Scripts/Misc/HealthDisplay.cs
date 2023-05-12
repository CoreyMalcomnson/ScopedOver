using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.UI;

public class HealthDisplay : NetworkBehaviour
{
    [SerializeField] private Health health;
    [SerializeField] private Image healthBar;

    override public void OnNetworkSpawn()
    {
        NetworkVariable<int> healthNetworkVar = health.GetHealthNetworkVar();

        UpdateHealthBar(healthNetworkVar.Value);
        healthNetworkVar.OnValueChanged += OnHealthChanged;
    }

    private void OnHealthChanged(int previousValue, int newValue)
    {
        UpdateHealthBar(newValue);
    }

    private void UpdateHealthBar(int newHealth)
    {
        int maxHealth = health.GetMaxHealth();
        healthBar.fillAmount = Mathf.InverseLerp(0, maxHealth, newHealth);
    }
}
