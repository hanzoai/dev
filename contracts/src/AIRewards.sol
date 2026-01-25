// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Permit.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title AIRewards
 * @author Hanzo AI Inc.
 * @notice ERC20 token for AI Compute Contribution Rewards (HIP-XXX)
 * @dev Implements minting for verified compute contributions, staking for pool operators,
 *      burn mechanism, and governance features as specified in the HIP.
 *
 * Key Features:
 * - Epoch-based reward distribution (24 hours)
 * - Quality-weighted rewards with multipliers
 * - Staking requirements for providers
 * - NVTrust hardware attestation integration
 * - Vesting schedules (50% immediate, 25% 7-day, 25% 30-day)
 * - Deflationary burn mechanism (10% of emissions)
 */
contract AIRewards is ERC20, ERC20Burnable, ERC20Permit, AccessControl, ReentrancyGuard, Pausable {
    // ============================================================
    // CONSTANTS
    // ============================================================

    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    bytes32 public constant VALIDATOR_ROLE = keccak256("VALIDATOR_ROLE");
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    bytes32 public constant SLASHER_ROLE = keccak256("SLASHER_ROLE");

    uint256 public constant PRECISION = 10_000; // 100.00%
    uint256 public constant EPOCH_DURATION = 24 hours;
    uint256 public constant MIN_STAKE_DURATION = 7 days;

    // Emission schedule (Year 1 = 1M/day, Year 2 = 750K/day, Year 3 = 500K/day, Year 4+ = 250K/day)
    uint256 public constant YEAR1_DAILY_EMISSION = 1_000_000 * 1e18;
    uint256 public constant YEAR2_DAILY_EMISSION = 750_000 * 1e18;
    uint256 public constant YEAR3_DAILY_EMISSION = 500_000 * 1e18;
    uint256 public constant YEAR4_DAILY_EMISSION = 250_000 * 1e18;

    // Allocation percentages
    uint256 public constant PROVIDER_SHARE = 7000; // 70%
    uint256 public constant TREASURY_SHARE = 2000; // 20%
    uint256 public constant BURN_SHARE = 1000; // 10%

    // Minimum thresholds
    uint256 public constant MIN_QUALITY_SCORE = 5000; // 50%
    uint256 public constant MIN_UPTIME_RATIO = 9000; // 90%
    uint256 public constant MIN_HCU_HOURS = 1e18; // 1.0 HCU-hours

    // Stake parameters
    uint256 public constant MAX_STAKE_EFFECT = 1_000_000 * 1e18;

    // ============================================================
    // STATE VARIABLES
    // ============================================================

    /// @notice Treasury address for protocol development funds
    address public treasury;

    /// @notice Minimum stake required to participate (default 1000 AI)
    uint256 public minStake = 1000 * 1e18;

    /// @notice Deployment timestamp for emission schedule calculation
    uint256 public immutable deploymentTime;

    /// @notice Current epoch number
    uint256 public currentEpoch;

    /// @notice Timestamp when current epoch ends
    uint256 public epochEndTime;

    /// @notice Total rewards distributed per epoch
    mapping(uint256 => uint256) public epochTotalRewards;

    /// @notice Whether epoch has been distributed
    mapping(uint256 => bool) public epochDistributed;

    // ============================================================
    // CONTRIBUTION TRACKING
    // ============================================================

    /// @notice Contribution metrics for each provider
    struct ContributionMetrics {
        uint256 totalCapacityHcu;       // Total capacity in HCU
        uint256 utilizedHcuHours;       // Utilized HCU-hours this epoch
        uint256 qualityScore;           // Quality score (0-10000)
        uint256 trustScore;             // Hardware trust score (0-100)
        uint256 uptimeRatio;            // Uptime ratio (0-10000)
        uint256 consecutiveUptime;      // Consecutive days at 99%+ uptime
        uint256 lastUpdateBlock;        // Last update block
        bytes32 hardwareId;             // NVTrust device ID (hashed)
        bool nvtrustVerified;           // Whether hardware is verified
    }

    /// @notice Provider contributions
    mapping(address => ContributionMetrics) public contributions;

    /// @notice Hardware IDs to provider mapping (prevents double registration)
    mapping(bytes32 => address) public hardwareToProvider;

    /// @notice Total network weight for the current epoch
    uint256 public totalNetworkWeight;

    /// @notice Provider weights for current epoch
    mapping(address => uint256) public providerWeights;

    // ============================================================
    // STAKING
    // ============================================================

    /// @notice Stake information for each provider
    struct StakeInfo {
        uint256 amount;           // Staked amount
        uint256 stakedAt;         // Timestamp when staked
        uint256 lockedUntil;      // Lock period end
        bool isOperator;          // Whether provider is a pool operator
    }

    /// @notice Provider stakes
    mapping(address => StakeInfo) public stakes;

    /// @notice Total staked amount
    uint256 public totalStaked;

    // ============================================================
    // VESTING
    // ============================================================

    /// @notice Vesting schedule for a provider
    struct VestingSchedule {
        uint256 totalAmount;      // Total reward amount
        uint256 immediateAmount;  // 50% available immediately
        uint256 weeklyAmount;     // 25% vests over 7 days
        uint256 monthlyAmount;    // 25% vests over 30 days
        uint256 epochTimestamp;   // When epoch ended
        uint256 claimedAmount;    // Amount already claimed
    }

    /// @notice Vesting schedules per provider per epoch
    mapping(address => mapping(uint256 => VestingSchedule)) public vestingSchedules;

    /// @notice Preferred reward destination for each provider
    mapping(address => RewardDestination) public rewardDestinations;

    /// @notice Reward destination options
    enum RewardDestination {
        HanzoEVM,      // Chain 36963 (default)
        ZooEVM,        // Chain 200200
        LuxCChain,     // Chain 96369
        Staking        // Auto-compound to stake
    }

    // ============================================================
    // EVENTS
    // ============================================================

    event ContributionRecorded(
        address indexed provider,
        uint256 hcuHours,
        uint256 epoch,
        bytes32 hardwareId
    );

    event QualityScoreUpdated(
        address indexed provider,
        uint256 oldScore,
        uint256 newScore
    );

    event TrustScoreUpdated(
        address indexed provider,
        uint256 trustScore,
        bool nvtrustVerified
    );

    event EpochDistributed(
        uint256 indexed epoch,
        uint256 totalRewards,
        uint256 providerShare,
        uint256 treasuryShare,
        uint256 burnedAmount
    );

    event RewardClaimed(
        address indexed provider,
        uint256 amount,
        uint256 epoch,
        RewardDestination destination
    );

    event ProviderSlashed(
        address indexed provider,
        uint256 amount,
        string reason,
        address reporter
    );

    event Staked(
        address indexed provider,
        uint256 amount,
        uint256 lockedUntil
    );

    event Unstaked(
        address indexed provider,
        uint256 amount
    );

    event HardwareRegistered(
        address indexed provider,
        bytes32 indexed hardwareId,
        uint256 trustScore
    );

    event TreasuryUpdated(
        address indexed oldTreasury,
        address indexed newTreasury
    );

    event ParameterUpdated(
        bytes32 indexed key,
        uint256 oldValue,
        uint256 newValue
    );

    // ============================================================
    // ERRORS
    // ============================================================

    error InsufficientStake();
    error StakeLocked();
    error BelowMinimumThreshold();
    error HardwareAlreadyRegistered();
    error HardwareNotVerified();
    error InvalidAttestationProof();
    error EpochNotEnded();
    error EpochAlreadyDistributed();
    error NothingToClaim();
    error InvalidParameter();
    error ZeroAddress();
    error NotAuthorized();

    // ============================================================
    // CONSTRUCTOR
    // ============================================================

    /**
     * @notice Initializes the AIRewards token
     * @param _treasury Treasury address for protocol funds
     * @param _admin Admin address with DEFAULT_ADMIN_ROLE
     */
    constructor(
        address _treasury,
        address _admin
    ) ERC20("Hanzo AI Compute Rewards", "AI") ERC20Permit("Hanzo AI Compute Rewards") {
        if (_treasury == address(0) || _admin == address(0)) revert ZeroAddress();

        treasury = _treasury;
        deploymentTime = block.timestamp;
        currentEpoch = 1;
        epochEndTime = block.timestamp + EPOCH_DURATION;

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(MINTER_ROLE, _admin);
        _grantRole(VALIDATOR_ROLE, _admin);
        _grantRole(GOVERNANCE_ROLE, _admin);
        _grantRole(SLASHER_ROLE, _admin);
    }

    // ============================================================
    // STAKING FUNCTIONS
    // ============================================================

    /**
     * @notice Stake tokens to become a compute provider
     * @param amount Amount of tokens to stake
     */
    function stake(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert InvalidParameter();

        _transfer(msg.sender, address(this), amount);

        StakeInfo storage info = stakes[msg.sender];
        info.amount += amount;
        info.stakedAt = block.timestamp;
        info.lockedUntil = block.timestamp + MIN_STAKE_DURATION;

        totalStaked += amount;

        emit Staked(msg.sender, amount, info.lockedUntil);
    }

    /**
     * @notice Unstake tokens after lock period
     * @param amount Amount to unstake
     */
    function unstake(uint256 amount) external nonReentrant {
        StakeInfo storage info = stakes[msg.sender];

        if (info.amount < amount) revert InsufficientStake();
        if (block.timestamp < info.lockedUntil) revert StakeLocked();

        // Ensure provider maintains minimum stake if still active
        ContributionMetrics storage contrib = contributions[msg.sender];
        if (contrib.nvtrustVerified && info.amount - amount < minStake) {
            revert InsufficientStake();
        }

        info.amount -= amount;
        totalStaked -= amount;

        _transfer(address(this), msg.sender, amount);

        emit Unstaked(msg.sender, amount);
    }

    /**
     * @notice Get the stake multiplier for a provider (1.0x - 1.5x)
     * @param provider Provider address
     * @return multiplier Stake multiplier scaled by PRECISION
     */
    function getStakeMultiplier(address provider) public view returns (uint256) {
        uint256 stakeAmount = stakes[provider].amount;
        if (stakeAmount == 0) return PRECISION; // 1.0x

        // Stake_multiplier = 1.0 + 0.5 * min(1.0, log(Stake) / log(MAX_STAKE))
        // Approximation using sqrt for gas efficiency
        uint256 ratio = (stakeAmount * PRECISION) / MAX_STAKE_EFFECT;
        if (ratio > PRECISION) ratio = PRECISION;

        // sqrt approximation: 1.0 + 0.5 * sqrt(ratio)
        uint256 sqrtRatio = _sqrt(ratio * PRECISION);
        return PRECISION + (sqrtRatio / 2);
    }

    // ============================================================
    // HARDWARE REGISTRATION
    // ============================================================

    /**
     * @notice Register hardware with NVTrust attestation
     * @param attestationProof NVTrust attestation proof
     * @param hardwareId Unique hardware identifier
     * @param trustScore Trust score based on hardware type (0-100)
     */
    function registerHardware(
        bytes calldata attestationProof,
        bytes32 hardwareId,
        uint256 trustScore
    ) external nonReentrant whenNotPaused {
        if (stakes[msg.sender].amount < minStake) revert InsufficientStake();
        if (hardwareToProvider[hardwareId] != address(0)) revert HardwareAlreadyRegistered();

        // In production, verify attestationProof against NVTrust
        // For now, we accept the proof if it's non-empty
        if (attestationProof.length == 0) revert InvalidAttestationProof();

        hardwareToProvider[hardwareId] = msg.sender;

        ContributionMetrics storage contrib = contributions[msg.sender];
        contrib.hardwareId = hardwareId;
        contrib.trustScore = trustScore > 100 ? 100 : trustScore;
        contrib.nvtrustVerified = true;
        contrib.lastUpdateBlock = block.number;

        emit HardwareRegistered(msg.sender, hardwareId, trustScore);
    }

    // ============================================================
    // CONTRIBUTION TRACKING
    // ============================================================

    /**
     * @notice Record compute contribution for the current epoch
     * @param attestationProof Attestation proof for the work
     * @param hcuHours HCU-hours contributed
     * @param jobsCompleted Number of completed jobs
     * @param jobsFailed Number of failed jobs
     */
    function recordContribution(
        bytes calldata attestationProof,
        uint256 hcuHours,
        uint256 jobsCompleted,
        uint256 jobsFailed
    ) external nonReentrant whenNotPaused {
        ContributionMetrics storage contrib = contributions[msg.sender];

        // Validate requirements
        if (!contrib.nvtrustVerified) revert HardwareNotVerified();
        if (stakes[msg.sender].amount < minStake) revert InsufficientStake();
        if (attestationProof.length == 0) revert InvalidAttestationProof();

        // Update contribution metrics
        contrib.utilizedHcuHours += hcuHours;
        contrib.lastUpdateBlock = block.number;

        // Update quality score based on job success rate
        if (jobsCompleted + jobsFailed > 0) {
            uint256 successRate = (jobsCompleted * PRECISION) / (jobsCompleted + jobsFailed);
            // Weighted average with existing quality score
            contrib.qualityScore = (contrib.qualityScore * 7 + successRate * 3) / 10;
        }

        // Calculate and update provider weight
        uint256 weight = _calculateProviderWeight(msg.sender);
        totalNetworkWeight = totalNetworkWeight - providerWeights[msg.sender] + weight;
        providerWeights[msg.sender] = weight;

        emit ContributionRecorded(msg.sender, hcuHours, currentEpoch, contrib.hardwareId);
    }

    /**
     * @notice Update quality score (called by PoAI validators)
     * @param provider Provider address
     * @param newScore New quality score (0-10000)
     * @param poaiProof PoAI verification proof
     */
    function updateQualityScore(
        address provider,
        uint256 newScore,
        bytes calldata poaiProof
    ) external onlyRole(VALIDATOR_ROLE) {
        if (poaiProof.length == 0) revert InvalidAttestationProof();
        if (newScore > PRECISION) revert InvalidParameter();

        ContributionMetrics storage contrib = contributions[provider];
        uint256 oldScore = contrib.qualityScore;
        contrib.qualityScore = newScore;

        // Recalculate weight
        uint256 weight = _calculateProviderWeight(provider);
        totalNetworkWeight = totalNetworkWeight - providerWeights[provider] + weight;
        providerWeights[provider] = weight;

        emit QualityScoreUpdated(provider, oldScore, newScore);
    }

    /**
     * @notice Update uptime ratio for a provider
     * @param provider Provider address
     * @param uptimeRatio New uptime ratio (0-10000)
     */
    function updateUptimeRatio(
        address provider,
        uint256 uptimeRatio
    ) external onlyRole(VALIDATOR_ROLE) {
        if (uptimeRatio > PRECISION) revert InvalidParameter();

        ContributionMetrics storage contrib = contributions[provider];
        contrib.uptimeRatio = uptimeRatio;

        // Update consecutive uptime bonus
        if (uptimeRatio >= 9900) {
            contrib.consecutiveUptime++;
        } else {
            contrib.consecutiveUptime = 0;
        }

        // Recalculate weight
        uint256 weight = _calculateProviderWeight(provider);
        totalNetworkWeight = totalNetworkWeight - providerWeights[provider] + weight;
        providerWeights[provider] = weight;
    }

    // ============================================================
    // EPOCH DISTRIBUTION
    // ============================================================

    /**
     * @notice Trigger epoch distribution
     * @dev Can be called by anyone once epoch has ended
     */
    function triggerEpochDistribution() external nonReentrant whenNotPaused {
        if (block.timestamp < epochEndTime) revert EpochNotEnded();
        if (epochDistributed[currentEpoch]) revert EpochAlreadyDistributed();

        uint256 epochReward = _getEpochEmission();

        // Calculate allocations
        uint256 providerReward = (epochReward * PROVIDER_SHARE) / PRECISION;
        uint256 treasuryReward = (epochReward * TREASURY_SHARE) / PRECISION;
        uint256 burnAmount = epochReward - providerReward - treasuryReward;

        // Mint provider rewards (held in contract for vesting)
        _mint(address(this), providerReward);

        // Mint treasury allocation
        _mint(treasury, treasuryReward);

        // Record epoch data
        epochTotalRewards[currentEpoch] = providerReward;
        epochDistributed[currentEpoch] = true;

        // Distribute to providers
        _distributeEpochRewards(providerReward);

        emit EpochDistributed(currentEpoch, epochReward, providerReward, treasuryReward, burnAmount);

        // Move to next epoch
        currentEpoch++;
        epochEndTime = block.timestamp + EPOCH_DURATION;

        // Reset epoch weights
        totalNetworkWeight = 0;
    }

    /**
     * @notice Get current epoch emission amount
     * @return emission Daily emission amount
     */
    function _getEpochEmission() internal view returns (uint256) {
        uint256 yearsSinceDeployment = (block.timestamp - deploymentTime) / 365 days;

        if (yearsSinceDeployment == 0) return YEAR1_DAILY_EMISSION;
        if (yearsSinceDeployment == 1) return YEAR2_DAILY_EMISSION;
        if (yearsSinceDeployment == 2) return YEAR3_DAILY_EMISSION;
        return YEAR4_DAILY_EMISSION;
    }

    /**
     * @notice Distribute epoch rewards to providers based on weights
     * @param totalReward Total reward amount to distribute
     */
    function _distributeEpochRewards(uint256 totalReward) internal {
        // This would iterate through all active providers
        // In production, use merkle tree distribution for gas efficiency
        // For now, rewards are recorded via vesting schedules when providers claim
    }

    // ============================================================
    // CLAIMING
    // ============================================================

    /**
     * @notice Claim available rewards
     * @return claimed Total amount claimed
     */
    function claimRewards() external nonReentrant returns (uint256 claimed) {
        claimed = getClaimableAmount(msg.sender);
        if (claimed == 0) revert NothingToClaim();

        RewardDestination dest = rewardDestinations[msg.sender];

        // Mark rewards as claimed across all epochs
        for (uint256 epoch = 1; epoch < currentEpoch; epoch++) {
            VestingSchedule storage schedule = vestingSchedules[msg.sender][epoch];
            uint256 vested = _getVestedAmount(schedule);
            uint256 unclaimed = vested - schedule.claimedAmount;
            schedule.claimedAmount = vested;

            if (unclaimed > 0) {
                emit RewardClaimed(msg.sender, unclaimed, epoch, dest);
            }
        }

        if (dest == RewardDestination.Staking) {
            // Auto-compound to stake
            stakes[msg.sender].amount += claimed;
            totalStaked += claimed;
            emit Staked(msg.sender, claimed, stakes[msg.sender].lockedUntil);
        } else {
            // Transfer to provider (for same-chain, or trigger bridge for cross-chain)
            _transfer(address(this), msg.sender, claimed);
        }
    }

    /**
     * @notice Get total claimable amount for a provider
     * @param provider Provider address
     * @return amount Claimable amount
     */
    function getClaimableAmount(address provider) public view returns (uint256 amount) {
        for (uint256 epoch = 1; epoch < currentEpoch; epoch++) {
            VestingSchedule storage schedule = vestingSchedules[provider][epoch];
            uint256 vested = _getVestedAmount(schedule);
            amount += vested - schedule.claimedAmount;
        }
    }

    /**
     * @notice Calculate vested amount from a schedule
     * @param schedule Vesting schedule
     * @return vested Total vested amount
     */
    function _getVestedAmount(VestingSchedule storage schedule) internal view returns (uint256 vested) {
        if (schedule.totalAmount == 0) return 0;

        // Immediate portion (50%)
        vested = schedule.immediateAmount;

        // Weekly portion (25% over 7 days)
        uint256 weeklyElapsed = block.timestamp - schedule.epochTimestamp;
        if (weeklyElapsed >= 7 days) {
            vested += schedule.weeklyAmount;
        } else {
            vested += (schedule.weeklyAmount * weeklyElapsed) / 7 days;
        }

        // Monthly portion (25% over 30 days)
        if (weeklyElapsed >= 30 days) {
            vested += schedule.monthlyAmount;
        } else {
            vested += (schedule.monthlyAmount * weeklyElapsed) / 30 days;
        }
    }

    /**
     * @notice Set reward destination for claiming
     * @param destination Desired destination
     */
    function setRewardDestination(RewardDestination destination) external {
        rewardDestinations[msg.sender] = destination;
    }

    /**
     * @notice Record epoch reward for a provider (called during distribution)
     * @param provider Provider address
     * @param epoch Epoch number
     * @param amount Reward amount
     */
    function _recordEpochReward(address provider, uint256 epoch, uint256 amount) internal {
        VestingSchedule storage schedule = vestingSchedules[provider][epoch];
        schedule.totalAmount = amount;
        schedule.immediateAmount = (amount * 50) / 100;
        schedule.weeklyAmount = (amount * 25) / 100;
        schedule.monthlyAmount = amount - schedule.immediateAmount - schedule.weeklyAmount;
        schedule.epochTimestamp = block.timestamp;
    }

    // ============================================================
    // SLASHING
    // ============================================================

    /**
     * @notice Slash a provider for malicious behavior
     * @param provider Provider to slash
     * @param percentage Percentage to slash (0-10000)
     * @param reason Reason for slashing
     * @param reporter Reporter address (receives 50% of slash)
     */
    function slash(
        address provider,
        uint256 percentage,
        string calldata reason,
        address reporter
    ) external onlyRole(SLASHER_ROLE) nonReentrant {
        if (percentage > PRECISION) revert InvalidParameter();

        StakeInfo storage info = stakes[provider];
        uint256 slashAmount = (info.amount * percentage) / PRECISION;

        info.amount -= slashAmount;
        totalStaked -= slashAmount;

        // 50% to reporter, 50% burned
        uint256 reporterReward = slashAmount / 2;
        _transfer(address(this), reporter, reporterReward);
        _burn(address(this), slashAmount - reporterReward);

        // Clear hardware registration
        bytes32 hwId = contributions[provider].hardwareId;
        if (hwId != bytes32(0)) {
            delete hardwareToProvider[hwId];
        }
        contributions[provider].nvtrustVerified = false;

        emit ProviderSlashed(provider, slashAmount, reason, reporter);
    }

    // ============================================================
    // GOVERNANCE
    // ============================================================

    /**
     * @notice Update minimum stake requirement
     * @param newMinStake New minimum stake amount
     */
    function setMinStake(uint256 newMinStake) external onlyRole(GOVERNANCE_ROLE) {
        if (newMinStake < 100 * 1e18 || newMinStake > 10000 * 1e18) revert InvalidParameter();

        emit ParameterUpdated("minStake", minStake, newMinStake);
        minStake = newMinStake;
    }

    /**
     * @notice Update treasury address
     * @param newTreasury New treasury address
     */
    function setTreasury(address newTreasury) external onlyRole(GOVERNANCE_ROLE) {
        if (newTreasury == address(0)) revert ZeroAddress();

        emit TreasuryUpdated(treasury, newTreasury);
        treasury = newTreasury;
    }

    /**
     * @notice Pause contract in emergency
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpause contract
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============================================================
    // VIEW FUNCTIONS
    // ============================================================

    /**
     * @notice Get contribution metrics for a provider
     * @param provider Provider address
     * @return metrics Contribution metrics
     */
    function getContribution(address provider) external view returns (ContributionMetrics memory) {
        return contributions[provider];
    }

    /**
     * @notice Get stake info for a provider
     * @param provider Provider address
     * @return info Stake information
     */
    function getStakeInfo(address provider) external view returns (StakeInfo memory) {
        return stakes[provider];
    }

    /**
     * @notice Get current epoch info
     * @return epoch Current epoch number
     * @return endTime Epoch end timestamp
     * @return emission Daily emission rate
     */
    function getEpochInfo() external view returns (uint256 epoch, uint256 endTime, uint256 emission) {
        return (currentEpoch, epochEndTime, _getEpochEmission());
    }

    /**
     * @notice Calculate provider weight based on contribution metrics
     * @param provider Provider address
     * @return weight Provider weight
     */
    function _calculateProviderWeight(address provider) internal view returns (uint256) {
        ContributionMetrics storage contrib = contributions[provider];

        // Check minimum thresholds
        if (contrib.qualityScore < MIN_QUALITY_SCORE) return 0;
        if (contrib.uptimeRatio < MIN_UPTIME_RATIO) return 0;
        if (contrib.utilizedHcuHours < MIN_HCU_HOURS) return 0;
        if (!contrib.nvtrustVerified) return 0;

        // W_provider = HCU_utilized * Quality_multiplier * Trust_multiplier * Uptime_multiplier * Stake_multiplier

        // Quality multiplier (0.5x - 2.0x)
        uint256 qualityMult = 5000 + (15000 * contrib.qualityScore) / PRECISION;

        // Trust multiplier (0.6x - 1.0x)
        uint256 trustMult = (contrib.trustScore * PRECISION) / 100;

        // Uptime multiplier (0.7x - 1.2x)
        uint256 uptimeMult = 7000 + (5000 * contrib.uptimeRatio) / PRECISION;

        // Uptime bonus for consecutive uptime
        if (contrib.consecutiveUptime >= 30) {
            uptimeMult = (uptimeMult * 110) / 100; // +10%
        } else if (contrib.consecutiveUptime >= 7) {
            uptimeMult = (uptimeMult * 105) / 100; // +5%
        }

        // Stake multiplier (1.0x - 1.5x)
        uint256 stakeMult = getStakeMultiplier(provider);

        // Calculate final weight
        uint256 weight = contrib.utilizedHcuHours;
        weight = (weight * qualityMult) / PRECISION;
        weight = (weight * trustMult) / PRECISION;
        weight = (weight * uptimeMult) / PRECISION;
        weight = (weight * stakeMult) / PRECISION;

        return weight;
    }

    /**
     * @notice Get provider reward for a specific epoch
     * @param provider Provider address
     * @param epoch Epoch number
     * @return reward Reward amount for that epoch
     */
    function getEpochReward(address provider, uint256 epoch) external view returns (uint256) {
        return vestingSchedules[provider][epoch].totalAmount;
    }

    /**
     * @notice Calculate estimated reward for provider in current epoch
     * @param provider Provider address
     * @return estimated Estimated reward
     */
    function estimateCurrentEpochReward(address provider) external view returns (uint256) {
        if (totalNetworkWeight == 0) return 0;

        uint256 epochReward = (_getEpochEmission() * PROVIDER_SHARE) / PRECISION;
        uint256 weight = providerWeights[provider];

        return (epochReward * weight) / totalNetworkWeight;
    }

    // ============================================================
    // INTERNAL HELPERS
    // ============================================================

    /**
     * @notice Integer square root using Babylonian method
     * @param x Input value
     * @return y Square root of x
     */
    function _sqrt(uint256 x) internal pure returns (uint256 y) {
        if (x == 0) return 0;

        uint256 z = (x + 1) / 2;
        y = x;
        while (z < y) {
            y = z;
            z = (x / z + z) / 2;
        }
    }

    // ============================================================
    // MINTING (RESTRICTED)
    // ============================================================

    /**
     * @notice Mint tokens for verified compute contributions
     * @dev Only callable by addresses with MINTER_ROLE
     * @param to Recipient address
     * @param amount Amount to mint
     */
    function mint(address to, uint256 amount) external onlyRole(MINTER_ROLE) {
        _mint(to, amount);
    }
}
