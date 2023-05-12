using System;
using System.Collections;
using Unity.Netcode;
using UnityEngine;
using UnityEngine.AI;

public class AnimalMover : NetworkBehaviour
{
    private const float DEFAULT_STOPPING_DISTANCE = 0.5f;

    private NavMeshAgent navMeshAgent;
    private bool isNavigating;
    private Vector3 targetPosition;
    private float stoppingDistance;

    private void Awake()
    {
        navMeshAgent = GetComponent<NavMeshAgent>();
    }

    public override void OnNetworkSpawn()
    {
        if (!IsServer)
        {
            this.enabled = false;
            return;
        }
    }

    private void Update()
    {
        if (!IsSpawned) return;
        if (!isNavigating) return;

        if (navMeshAgent.isOnOffMeshLink)
        {
            navMeshAgent.CompleteOffMeshLink();
            MoveTo(targetPosition, stoppingDistance);
        }

        if (HasReachedTargetPosition())
        {
            Stop();
        }
    }

    private bool HasReachedTargetPosition()
    {
        return Vector3.Distance(transform.position, targetPosition) <= stoppingDistance;
    }

    public void MoveTo(Vector3 targetPosition, float stoppingDistance = DEFAULT_STOPPING_DISTANCE)
    {
        isNavigating = true;
        this.targetPosition = targetPosition;
        this.stoppingDistance = stoppingDistance;

        navMeshAgent.isStopped = false;
        navMeshAgent.SetDestination(targetPosition);
    }

    public void Stop()
    {
        isNavigating = false;
        navMeshAgent.isStopped = true;
    }

    public bool IsNavigating()
    {
        return isNavigating;
    }
}