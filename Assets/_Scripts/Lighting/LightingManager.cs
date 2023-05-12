using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[ExecuteAlways]
public class LightingManager : MonoBehaviour
{
    // 
    [SerializeField] private Light directionalLight;
    [SerializeField] private LightingPreset lightingPreset;

    [SerializeField, Range(0, 24)] private float timeOfDay = 12;
    [SerializeField] private float secondsPerHour = 5;

    private void Update()
    {
        if (Application.isPlaying)
        {
            timeOfDay += Time.deltaTime / secondsPerHour;
            timeOfDay %= 24;

            UpdateLighting(timeOfDay);
        }    
    }

    private void UpdateLighting(float time)
    {
        if (lightingPreset == null) { return; }
        if (directionalLight == null) { return; }

        float timePercent = time / 24f;

            RenderSettings.ambientLight = lightingPreset.AmbientColor.Evaluate(timePercent);
        RenderSettings.fogColor = lightingPreset.FogColor.Evaluate(timePercent);

        
        directionalLight.color = lightingPreset.DirectionalColor.Evaluate(timePercent);
        directionalLight.transform.localRotation =
            Quaternion.Euler(new Vector3(timePercent * 360f - 90f, 170f, 0));
           
    }

    private void OnValidate()
    {
        if (directionalLight == null)
            Debug.LogError("DirectionalLight is null");

        if (lightingPreset == null)
            Debug.LogError("LightingPreset is null");

        UpdateLighting(timeOfDay);
    }
}
