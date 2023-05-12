using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class CharacterAudio : NetworkBehaviour
{
    [SerializeField] private AudioClip attackAudioClip;
    [SerializeField] private float attackPitchMin = 0.45f;
    [SerializeField] private float attackPitchMax = 0.55f;
    [SerializeField] private float attackVolume = 1.5f;

    [SerializeField] private AudioClip deathAudioClip;
    [SerializeField] private float deathPitchMin = 0.45f;
    [SerializeField] private float deathPitchMax = 0.55f;
    [SerializeField] private float deathVolume = 2f;

    [SerializeField] private AudioClip hitAudioClip;
    [SerializeField] private float hitPitchMin = 0.45f;
    [SerializeField] private float hitPitchMax = 0.55f;
    [SerializeField] private float hitVolume = 2f;

    public override void OnNetworkSpawn()
    {
        if (IsOwner)
        {
            WeaponManager.OnAttack += OnAttack;
            Character.Local.Health.OnDeath.AddListener(OnDeath);
            Character.Local.Health.GetHealthNetworkVar().OnValueChanged += OnHealthChanged;
        }
    }

    public override void OnNetworkDespawn()
    {
        if (IsOwner)
        {
            WeaponManager.OnAttack -= OnAttack;
            Character.Local.Health.OnDeath.RemoveListener(OnDeath);
            Character.Local.Health.GetHealthNetworkVar().OnValueChanged -= OnHealthChanged;
        }
    }

    private void OnAttack(int combo)
    {
        AudioManager.Instance.PlayOneShot3DSoundEffectServerRPC(
            attackAudioClip.name,
            transform.position,
            Random.Range(attackPitchMin, attackPitchMax),
            attackVolume
        );
    }

    private void OnDeath()
    {
        AudioManager.Instance.PlayOneShot3DSoundEffectServerRPC(
            deathAudioClip.name,
            transform.position,
            Random.Range(deathPitchMin, deathPitchMax),
            deathVolume
        );
    }

    private void OnHealthChanged(int oldHealth, int newHealth)
    {
        if (newHealth < oldHealth)
        {
            AudioManager.Instance.PlayOneShot3DSoundEffectServerRPC(
            hitAudioClip.name,
            transform.position,
            Random.Range(hitPitchMin, hitPitchMax),
            hitVolume
        );
        }
    }
}
