using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MusicManager : MonoBehaviour
{
    private void Start()
    {
        AudioManager.Instance.Play2DMusic(AudioManager.Instance.GetClipByName("Music"), true);
    }
}
