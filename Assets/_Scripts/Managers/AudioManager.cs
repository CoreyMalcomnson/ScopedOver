using System;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;

public class AudioManager : NetworkBehaviour
{
    public static AudioManager Instance { get; private set; }

    [SerializeField] private AudioSource musicAudioSource;

    [SerializeField, Range(0, 1)] private float masterVolume = 1;

    [SerializeField, Range(0, 1)] private float soundEffectVolume = 1;
    [SerializeField, Range(0, 1)] private float musicVolume = 1;

    [SerializeField] private List<AudioClip> audioClipList;
    private Dictionary<string, AudioClip> audioClipDictionary;

    private void Awake()
    {
        SetupSingleton();
        PopulateDictionary();
    }

    private void PopulateDictionary()
    {
        audioClipDictionary = new();
        foreach (AudioClip audioClip in audioClipList)
        {
            audioClipDictionary.Add(audioClip.name, audioClip);
        }
    }

    private void SetupSingleton()
    {
        if (Instance == null)
        {
            Instance = this;
            DontDestroyOnLoad(this);
        }
        else
        {
            Destroy(gameObject);
        }
    }

    private void UpdateAudioSourcesVolume()
    {
        musicAudioSource.volume = masterVolume * musicVolume;
    }

    public void PlayOneShot3DSoundEffect(AudioClip audioClip, Vector3 position, float pitch = 1f, float volumeScale = 1f)
    {
        var go = new GameObject("Sound", typeof(AudioSource));
        go.transform.position = position;

        var audioSource = go.GetComponent<AudioSource>();
        audioSource.spatialBlend = 1f; // 3D audio
        audioSource.pitch = pitch;
        audioSource.volume = masterVolume * soundEffectVolume;
        audioSource.PlayOneShot(audioClip, volumeScale);

        Destroy(go, audioClip.length);
    }

    [ServerRpc(RequireOwnership = false)]
    public void PlayOneShot3DSoundEffectServerRPC(string audioClipName, Vector3 position, float pitch = 1f, float volumeScale = 1f)
    {
        PlayOneShot3DSoundEffectClientRPC(audioClipName, position, pitch, volumeScale);
    }

    [ClientRpc]
    public void PlayOneShot3DSoundEffectClientRPC(string audioClipName, Vector3 position, float pitch = 1f, float volumeScale = 1f)
    {
        PlayOneShot3DSoundEffect(audioClipName, position, pitch, volumeScale);
    }

    public void PlayOneShot3DSoundEffect(string audioName, Vector3 position, float pitch = 1f, float volumeScale = 1f)
    {
        AudioClip audioClip = GetClipByName(audioName);

        if (audioClip)
        {
            PlayOneShot3DSoundEffect(audioClip, position, pitch, volumeScale);
        }
    }

    public void PlayOneShot2DSoundEffect(AudioClip audioClip, float pitch = 1f, float volumeScale = 1f)
    {
        var go = new GameObject("Sound", typeof(AudioSource));

        var audioSource = go.GetComponent<AudioSource>();
        audioSource.spatialBlend = 0f; // 2D audio
        audioSource.pitch = pitch;
        audioSource.volume = masterVolume * soundEffectVolume;
        audioSource.PlayOneShot(audioClip, volumeScale);

        Destroy(go, audioClip.length);
    }

    [ServerRpc(RequireOwnership = false)]
    public void PlayOneShot2DSoundEffectServerRPC(string audioClipName, float pitch = 1f, float volumeScale = 1f)
    {
        PlayOneShot2DSoundEffectClientRPC(audioClipName, pitch, volumeScale);
    }

    [ClientRpc]
    public void PlayOneShot2DSoundEffectClientRPC(string audioClipName, float pitch = 1f, float volumeScale = 1f)
    {
        PlayOneShot2DSoundEffect(audioClipName, pitch, volumeScale);
    }

    public void PlayOneShot2DSoundEffect(string audioName, float pitch = 1f, float volumeScale = 1f)
    {
        AudioClip audioClip = GetClipByName(audioName);

        if (audioClip)
        {
            PlayOneShot2DSoundEffect(audioClip, pitch, volumeScale);
        }
    }

    public void Play2DMusic(AudioClip audioClip, bool loop = false)
    {
        musicAudioSource.loop = loop;
        musicAudioSource.clip = audioClip;
        musicAudioSource.Play();
    }

    public AudioClip GetClipByName(string clipName)
    {
        bool hasClip = audioClipDictionary.TryGetValue(clipName, out AudioClip clip);
        return hasClip ? clip : null;
    }

    private void OnValidate()
    {
        UpdateAudioSourcesVolume();
    }
}