using UnityEngine;
using System.Collections;
using Unity.Netcode;
using System.Collections.Generic;

public class BearManager : NetworkBehaviour
{
    public static BearManager Instance;

    [SerializeField] private Bear bearPrefab;

    private List<Bear> bearList;

    public override void OnNetworkSpawn()
    {
        if (IsServer)
        {
            Instance = this;
            bearList = new();
        }
    }

    public void SpawnBears(Vector3 position, int numberOfBears)
    {
        if (numberOfBears < 0 || numberOfBears > 10000)
            return;

        for (int i = 0; i < numberOfBears; i++)
        {
            Bear bear = Instantiate(
            bearPrefab,
            position + new Vector3(
                Random.Range(-5, 5),
                position.y,
                Random.Range(-5, 5)),
            Quaternion.identity);

            bear.NetworkObject.Spawn();
        }
    }

    public void ClearBears()
    {
        while (bearList.Count > 0)
        {
            Bear bear = bearList[0];
            bear.NetworkObject.Despawn(true);
        }

        bearList.Clear();
    }

    public void AddBear(Bear bear)
    {
        bearList.Add(bear);
    }

    public void RemoveBear(Bear bear)
    {
        bearList.Remove(bear);
    }
}
