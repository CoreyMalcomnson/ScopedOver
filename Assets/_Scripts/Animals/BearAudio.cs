using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System;
using System.Collections.Generic;
using Random = UnityEngine.Random;

public class BearAudio : NetworkBehaviour
{
    [SerializeField] private AudioClip attackAudioClip;
    [SerializeField] private float attackPitchMin = 0.45f;
    [SerializeField] private float attackPitchMax = 0.55f;
    [SerializeField] private float attackVolume = 1.5f;

    [SerializeField] private AudioClip deathAudioClip;
    [SerializeField] private float deathPitchMin = 0.45f;
    [SerializeField] private float deathPitchMax = 0.55f;
    [SerializeField] private float deathVolume = 2f;

    [SerializeField] private List<AudioClip> hitAudioClips;
    [SerializeField] private float hitPitchMin = 0.45f;
    [SerializeField] private float hitPitchMax = 0.55f;
    [SerializeField] private float hitVolume = 2f;

    private Bear bear;

    public override void OnNetworkSpawn()
    {
        if (IsServer)
        {
            bear = GetComponent<Bear>();
            bear.OnAttack += OnAttack;
            bear.OnDeath += OnDeath;
            bear.Health.GetHealthNetworkVar().OnValueChanged += OnHealthChanged;
        }
    }

    public override void OnNetworkDespawn()
    {
        if (IsServer)
        {
            bear.OnAttack -= OnAttack;
            bear.OnDeath -= OnDeath;
            bear.Health.GetHealthNetworkVar().OnValueChanged -= OnHealthChanged;
        }
    }

    private void OnAttack()
    {
        AudioManager.Instance.PlayOneShot3DSoundEffectClientRPC(
            attackAudioClip.name,
            transform.position,
            Random.Range(attackPitchMin, attackPitchMax),
            attackVolume
        );
    }

    private void OnDeath()
    {
        AudioManager.Instance.PlayOneShot3DSoundEffectClientRPC(
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
            foreach (AudioClip audioClip in hitAudioClips)
            {
                AudioManager.Instance.PlayOneShot3DSoundEffectClientRPC(
                    audioClip.name,
                    transform.position,
                    Random.Range(hitPitchMin, hitPitchMax),
                    hitVolume
                );
            }
        }
    }
}
