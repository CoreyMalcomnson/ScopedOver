using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SequenceSoundPlayer : MonoBehaviour
{
    public List<AudioSource> sounds;
    private int index = 0;

    public void PlaySoundOfCurrentIndex()
    {
        sounds[index].Play();
        index++;

        if (index >= sounds.Count)
            index = 0;

    }
}
