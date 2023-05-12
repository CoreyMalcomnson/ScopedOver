using System.Collections;
using System.Collections.Generic;
using System.Threading.Tasks;
using UnityEngine;
using Random = UnityEngine.Random;

public class ResourceSpawner : MonoBehaviour
{
    [System.Serializable]
    struct ResourceInfo
    {
        public List<Transform> ResourcePrefabList;
        public float ResourceAmount;
        public float SpawnRange;
        public float MinimumDistance;
        public float MaxPositioningAttempts;

        public LayerMask GroundLayerMask;
        public LayerMask ObstructionLayerMask;
    }

    [SerializeField] private List<ResourceInfo> resouceInfoList;

    public bool IsBusy { get; private set; }

    [ContextMenu("RespawnAllResources")]
    public async void RespawnAllResources()
    {
        IsBusy = true;

        // Clear
        while (transform.childCount > 0)
        {
            DestroyImmediate(transform.GetChild(0).gameObject);
        }

        // Spawn
        foreach (ResourceInfo resourceInfo in resouceInfoList)
        {
            for (int i = 0; i < resourceInfo.ResourceAmount; i++)
            {
                if (!TrySpawnResource(resourceInfo)) break;
            }
        }
        

        IsBusy = false;
    }

    private bool TrySpawnResource(ResourceInfo resourceInfo)
    {
        bool validPosition = false;
        Vector3 position = default;
        int attempts = 0;
        while (validPosition == false && attempts < resourceInfo.MaxPositioningAttempts)
        {
            position = GetRandomPosition(resourceInfo);
            print(position);
            validPosition = !PositionHasObstructions(resourceInfo, position);
            attempts++;
        }

        if (validPosition)
        {
            Transform resource = Instantiate(resourceInfo.ResourcePrefabList.GetRandomElement(), transform);
            resource.position = position;
            return true;
        }

        return false;
    }

    private Vector3 GetRandomPosition(ResourceInfo resourceInfo)
    {
        Vector3 randomOffset = new Vector3(
            Random.Range(-resourceInfo.SpawnRange, resourceInfo.SpawnRange),
            100,
            Random.Range(-resourceInfo.SpawnRange, resourceInfo.SpawnRange));

        Ray ray = new Ray(transform.position + randomOffset, Vector3.down);
        Physics.Raycast(ray, out RaycastHit hitInfo, 1000f, resourceInfo.GroundLayerMask);

        return hitInfo.point;
    }

    private bool PositionHasObstructions(ResourceInfo resourceInfo, Vector3 position)
    {
        foreach (Collider collider in Physics.OverlapSphere(position, resourceInfo.MinimumDistance))
        {
            if (resourceInfo.ObstructionLayerMask == (resourceInfo.ObstructionLayerMask | (1 << collider.gameObject.layer)))
            {
                return true;
            }
        }

        return false;
    }
}