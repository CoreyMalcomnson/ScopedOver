using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Netcode;
using UnityEngine;
using Random = UnityEngine.Random;

public class Bear : NetworkBehaviour
{
    public event Action OnAttack;
    public event Action OnDeath;
    public event Action OnDespawn;

    public Health Health { get; private set; }

    [SerializeField] private float attackDelay = 1f;
    [SerializeField] private float attackRange = 3f;
    [SerializeField] private float rotationSpeed = 5f;
    [SerializeField] private LayerMask entityLayerMask; 

    [SerializeField] private float despawnTime = 5f;

    [SerializeField] private float aggroRange = 5f;
    [SerializeField] private float chaseRange = 12f;
    [SerializeField] private int attackDamage = 33;

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
        Attacking = 4,
        Dead = 5,
    }

    private State state;
    private Vector3 startingPosition;
    private AnimalMover mover;

    private float timer;

    private void Awake()
    {
        mover = GetComponent<AnimalMover>();
        Health = GetComponent<Health>();
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

    public override void OnNetworkDespawn()
    {
        if (!IsServer)
        {
            return;
        }

        BearManager.Instance.RemoveBear(this);
    }

    private void Start()
    {
        if (!IsServer)
        {
            return;
        }
        BearManager.Instance.AddBear(this);
    }

    private void Update()
    {
        if (!IsSpawned) return;

        timer -= Time.deltaTime;

        // Update
        if (target != null && target.Health.IsDead)
        {
            target = null;
            SwitchState(GetRandomPassiveState());
            return;
        }

        if (state != State.Dead && Health.GetHealthNetworkVar().Value == 0)
        {
            SwitchState(State.Dead);
            return;
        }

        if (state == State.Sleeping || state == State.Idle || state == State.Wandering)
        {
            if (TryFindCharacterInAggroRange(out Character character))
            {
                target = character;
                SwitchState(State.Chasing);
                return;
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
                    // Collision Detection
                    Collider[] colliders = Physics.OverlapSphere(transform.position + transform.forward * attackRange /2f, attackRange / 2f, entityLayerMask);
                    foreach (Collider collider in colliders)
                    {
                        if (!collider.TryGetComponent(out Health health))
                            return;

                        if (health.NetworkObjectId != NetworkObjectId)
                        {
                            health.Damage(NetworkObject, attackDamage);
                        }
                    }

                    //
                    OnAttack?.Invoke();
                    timer = attackDelay;
                }


                // Rotate towards target
                transform.forward = Vector3.Lerp(
                    transform.forward,
                    (target.transform.position - transform.position).normalized,
                    Time.deltaTime * rotationSpeed);

                break;
            case State.Dead:
                if (timer <= 0)
                {
                    OnDespawn?.Invoke();
                    NetworkObject.Despawn(true);
                }
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
            case State.Dead:
                Debug.Log("Enetered Dead");
                mover.Stop();
                timer = despawnTime;
                OnDeath?.Invoke();
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
            if (keyValuePair.Value.Health.IsDead) continue;

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

    public Vector3 GetVelocity()
    {
        return mover.GetVelocity();
    }

    public State GetState()
    {
        return state;
    }
    
}
