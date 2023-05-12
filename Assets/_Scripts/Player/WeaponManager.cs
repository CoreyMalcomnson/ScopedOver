using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class WeaponManager : NetworkBehaviour
{
    public static WeaponManager Local;

    public static event Action<int> OnAttack;

    [SerializeField] private float swingDelay = 0.75f;
    [SerializeField] private int attackDamage = 25;
    [SerializeField] private float weaponRange = 2f;

    [SerializeField] private LayerMask entityLayerMask;

    [SerializeField] private int comboMin = 0;
    [SerializeField] private float comboMax = 2;

    [SerializeField] private GameObject inHandWeapon;
    [SerializeField] private GameObject onBackWeapon;

    private NetworkVariable<bool> weaponStateNetworkVar = new NetworkVariable<bool>(writePerm: NetworkVariableWritePermission.Server);

    private float serverLastSwingTime;
    private float clientLastSwingTime;

    private int combo;

    public override void OnNetworkSpawn()
    {
        if (IsOwner)
        {
            Local = this;
        }

        UpdateWeaponVisual();
        weaponStateNetworkVar.OnValueChanged += WeaponStateChanged;
    }

    private void Update()
    {
        if (!IsSpawned) return;
        if (!IsOwner) return;
        if (Character.Local.Health.IsDead) return;

        if (InputManager.Local.ToggleWeapon)
        {
            ToggleWeaponServerRPC();
        }

        if (InputManager.Local.FireDown)
        {
            UseWeaponClient();
        }
    }

    #region Attacking
    private void UseWeaponClient()
    {
        if (!IsOwner) return;
        if (!weaponStateNetworkVar.Value) return;

        // Check swingTimer
        if (Time.time - clientLastSwingTime < swingDelay)
            return;

        clientLastSwingTime = Time.time;

        // Tell Server
        UseWeaponServerRPC();

        // Update Combo
        combo += 1;
        if (combo > comboMax)
            combo = comboMin;

        // Fire Event
        OnAttack?.Invoke(combo);
    }

    [ServerRpc(RequireOwnership = true)]
    private void UseWeaponServerRPC()
    {
        // Check swingTimer
        if (Time.time - serverLastSwingTime < swingDelay)
            return;

        serverLastSwingTime = Time.time;

        // Collision Detection
        Collider[] colliders = Physics.OverlapSphere(inHandWeapon.transform.position, weaponRange / 2f, entityLayerMask);
        foreach (Collider collider in colliders)
        {
            if (!collider.TryGetComponent(out Health health))
                return;

            if (health.NetworkObjectId != NetworkObjectId)
            {
                health.Damage(NetworkObject, attackDamage);

                // Track kill
                if (health.IsDead)
                {
                    Player killer = NetworkObjectManager.Instance.GetPlayer(OwnerClientId);
                    string killed = health.gameObject.name;

                    Player potentialKilledPlayer = NetworkObjectManager.Instance.GetPlayer(health.OwnerClientId);
                    if (potentialKilledPlayer != null)
                    {
                        killed = potentialKilledPlayer.GetUsernameNetworkVar().Value.ToString();
                    }

                    if (killer != null) Scoreboard.Instance.AddKill(killer, killed);
                }
            }
        }
    }
    #endregion

    #region Toggling
    [ServerRpc(RequireOwnership =true)]
    private void ToggleWeaponServerRPC()
    {
        weaponStateNetworkVar.Value = !weaponStateNetworkVar.Value;
    }

    private void WeaponStateChanged(bool previousValue, bool newValue)
    {
        UpdateWeaponVisual();
    }

    private void UpdateWeaponVisual()
    {
        if (weaponStateNetworkVar.Value)
        {
            onBackWeapon.SetActive(false);
            inHandWeapon.SetActive(true);
        } else
        {
            onBackWeapon.SetActive(true);
            inHandWeapon.SetActive(false);
        }
    }

    public bool GetWeaponState()
    {
        return weaponStateNetworkVar.Value;
    }
    #endregion

}
