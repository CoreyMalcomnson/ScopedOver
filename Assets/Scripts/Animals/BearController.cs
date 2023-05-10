using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using Random = UnityEngine.Random;

public class BearController : NetworkBehaviour
{
    public event Action OnAttack;

    [SerializeField] private float attackDelay = 1f;
    [SerializeField] private float attackRange = 3f;
    [SerializeField] private float rotationSpeed = 5f;

    [SerializeField] private float aggroRange = 5f;
    [SerializeField] private float chaseRange = 12f;
    [SerializeField] private float attackDamage = 3f;

    [SerializeField] private float wanderRange = 10f;

    [SerializeField] private float minSleepTime = 5f;
    [SerializeField] private float maxSleepTime = 10f;

    [SerializeField] private float minIdleTime = 2f;
    [SerializeField] private float maxIdleTime = 7f;

    [SerializeField] private float minWanderTime = 5f;
    [SerializeField] private float maxWanderTime = 20f;

    private Character target;

    public enum State
    {
        Idle = 0,
        Sleeping = 1,
        Wandering = 2,
        Chasing = 3,
        Attacking = 4
    }

    private State state;
    private Vector3 startingPosition;
    private AnimalMover mover;

    private float timer;

    private void Awake()
    {
        mover = GetComponent<AnimalMover>();
    }

    public override void OnNetworkSpawn()
    {
        if (!IsServer)
        {
            this.enabled = false;
            return;
        }

        startingPosition = transform.position;
        SwitchState(GetRandomPassiveState());
    }

    private void Update()
    {
        if (!IsSpawned) return;

        timer -= Time.deltaTime;

        // Update

        if (state == State.Sleeping || state == State.Idle || state == State.Wandering)
        {
            if (TryFindCharacterInAggroRange(out Character character))
            {
                target = character;
                SwitchState(State.Chasing);
            }
        }

        switch (state)
        {
            case State.Idle:
                if (timer <= 0)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                break;
            case State.Sleeping:
                if (timer <= 0)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                break;
            case State.Wandering:
                if (timer <= 0)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                if (!mover.IsNavigating())
                {
                    mover.MoveTo(startingPosition + new Vector3(
                        Random.Range(-wanderRange, wanderRange),
                        startingPosition.y,
                        Random.Range(-wanderRange, wanderRange)
                    ));
                }

                break;
            case State.Chasing:
                if (target == null)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                if (DistanceCheck(target.transform.position) <= attackRange)
                {
                    SwitchState(State.Attacking);
                    return;
                }

                if (DistanceCheck(target.transform.position) > chaseRange)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                mover.MoveTo(target.transform.position);

                break;
            case State.Attacking:
                if (target == null)
                {
                    SwitchState(GetRandomPassiveState());
                    return;
                }

                if (DistanceCheck(target.transform.position) > attackRange)
                {
                    SwitchState(State.Chasing);
                    return;
                }

                if (timer <= 0)
                {
                    Debug.Log("Attack");
                    OnAttack?.Invoke();
                    timer = attackDelay;
                }


                // Rotate towards target
                transform.forward = Vector3.Lerp(
                    transform.forward,
                    (target.transform.position - transform.position).normalized,
                    Time.deltaTime * rotationSpeed);

                break;
        }
    }

    private void SwitchState(State newState)
    {
        // Exit
        switch (state)
        {
            case State.Idle:

                break;
            case State.Sleeping:

                break;
            case State.Wandering:

                break;
            case State.Chasing:

                break;
            case State.Attacking:

                break;
        }

        state = newState;

        // Enter
        switch (state)
        {
            case State.Idle:
                Debug.Log("Enetered Idle");
                timer = Random.Range(minIdleTime, maxIdleTime);
                mover.Stop();

                break;
            case State.Sleeping:
                Debug.Log("Enetered Sleeping");
                timer = Random.Range(minSleepTime, maxSleepTime);
                mover.Stop();

                break;
            case State.Wandering:
                Debug.Log("Enetered Wandering");
                timer = Random.Range(minWanderTime, maxWanderTime);

                break;
            case State.Chasing:
                Debug.Log("Enetered Chasing");

                break;
            case State.Attacking:
                Debug.Log("Enetered Attacking");
                timer = attackDelay;
                mover.Stop();

                break;
        }
    }

    private State GetRandomPassiveState()
    {
        return (State) Random.Range(0, 2 + 1);
    }

    private bool TryFindCharacterInAggroRange(out Character character)
    {
        float smallestDistance = float.MaxValue;
        character = null;

        foreach (KeyValuePair<ulong, Character> keyValuePair in NetworkObjectManager.Instance.GetCharacterDictionary())
        {
            float distance  = DistanceCheck(keyValuePair.Value.transform.position);
            if (distance < smallestDistance && distance <= aggroRange)
            {
                character = keyValuePair.Value;
                smallestDistance = distance;
            }
        }

        return character != null;
    }

    private float DistanceCheck(Vector3 pos)
    {
        return (pos - transform.position).magnitude;
    }

    public State GetState()
    {
        return state;
    }
    
}
