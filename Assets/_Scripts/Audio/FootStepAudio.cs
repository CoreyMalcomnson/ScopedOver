using System.Collections.Generic;
using UnityEngine;

public class FootStepAudio : MonoBehaviour
{
    [SerializeField] private List<AudioClip> footStepAudioClipList;

    [SerializeField] private float pitchMin = 0.45f;
    [SerializeField] private float pitchMax = 0.55f;

    [SerializeField] private float stepDelay = 0.4f;

    [SerializeField] private float stepVolume = 1f;

    private float timeTilNextStep;
    private float magnitude;
    private Vector3 lastFramePosition;

    private void Update()
    {
        magnitude = ((transform.position - lastFramePosition) / Time.deltaTime).magnitude;
        bool isMoving = magnitude > 0.1f;

        lastFramePosition = transform.position;
        if (!isMoving) return;

        timeTilNextStep -= Time.deltaTime;
        if (timeTilNextStep > 0) return;

        timeTilNextStep = stepDelay;

        PlayStepAudio();



    }

    private void PlayStepAudio()
    {
        AudioManager.Instance.PlayOneShot3DSoundEffect(
            footStepAudioClipList.GetRandomElement(),
            transform.position,
            Random.Range(pitchMin, pitchMax),
            stepVolume
        );
    }
}

