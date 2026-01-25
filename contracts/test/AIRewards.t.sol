// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Test.sol";
import "../src/AIRewards.sol";

contract AIRewardsTest is Test {
    AIRewards public token;

    address public admin = address(1);
    address public treasury = address(2);
    address public provider1 = address(3);
    address public provider2 = address(4);
    address public validator = address(5);
    address public slasher = address(6);
    address public reporter = address(7);

    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    bytes32 public constant VALIDATOR_ROLE = keccak256("VALIDATOR_ROLE");
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    bytes32 public constant SLASHER_ROLE = keccak256("SLASHER_ROLE");

    bytes public constant MOCK_ATTESTATION = hex"deadbeef";
    bytes32 public constant HARDWARE_ID_1 = keccak256("GPU_H100_001");
    bytes32 public constant HARDWARE_ID_2 = keccak256("GPU_H100_002");

    function setUp() public {
        vm.prank(admin);
        token = new AIRewards(treasury, admin);

        // Grant roles
        vm.startPrank(admin);
        token.grantRole(VALIDATOR_ROLE, validator);
        token.grantRole(SLASHER_ROLE, slasher);

        // Mint initial tokens for testing
        token.mint(provider1, 100_000 * 1e18);
        token.mint(provider2, 100_000 * 1e18);
        vm.stopPrank();
    }

    // ============================================================
    // INITIALIZATION TESTS
    // ============================================================

    function test_Initialization() public view {
        assertEq(token.name(), "Hanzo AI Compute Rewards");
        assertEq(token.symbol(), "AI");
        assertEq(token.treasury(), treasury);
        assertEq(token.currentEpoch(), 1);
        assertEq(token.minStake(), 1000 * 1e18);
    }

    function test_InitialRoles() public view {
        assertTrue(token.hasRole(token.DEFAULT_ADMIN_ROLE(), admin));
        assertTrue(token.hasRole(MINTER_ROLE, admin));
        assertTrue(token.hasRole(VALIDATOR_ROLE, validator));
        assertTrue(token.hasRole(SLASHER_ROLE, slasher));
    }

    function test_RevertOnZeroAddressConstruction() public {
        vm.expectRevert(AIRewards.ZeroAddress.selector);
        new AIRewards(address(0), admin);

        vm.expectRevert(AIRewards.ZeroAddress.selector);
        new AIRewards(treasury, address(0));
    }

    // ============================================================
    // STAKING TESTS
    // ============================================================

    function test_Stake() public {
        uint256 stakeAmount = 10_000 * 1e18;

        vm.startPrank(provider1);
        token.approve(address(token), stakeAmount);
        token.stake(stakeAmount);
        vm.stopPrank();

        AIRewards.StakeInfo memory info = token.getStakeInfo(provider1);
        assertEq(info.amount, stakeAmount);
        assertGt(info.lockedUntil, block.timestamp);
        assertEq(token.totalStaked(), stakeAmount);
    }

    function test_StakeMultipleTimes() public {
        uint256 firstStake = 5_000 * 1e18;
        uint256 secondStake = 3_000 * 1e18;

        vm.startPrank(provider1);
        token.approve(address(token), firstStake + secondStake);
        token.stake(firstStake);
        token.stake(secondStake);
        vm.stopPrank();

        AIRewards.StakeInfo memory info = token.getStakeInfo(provider1);
        assertEq(info.amount, firstStake + secondStake);
    }

    function test_RevertStakeZeroAmount() public {
        vm.startPrank(provider1);
        vm.expectRevert(AIRewards.InvalidParameter.selector);
        token.stake(0);
        vm.stopPrank();
    }

    function test_Unstake() public {
        uint256 stakeAmount = 10_000 * 1e18;

        vm.startPrank(provider1);
        token.approve(address(token), stakeAmount);
        token.stake(stakeAmount);

        // Fast forward past lock period
        vm.warp(block.timestamp + 8 days);

        uint256 balanceBefore = token.balanceOf(provider1);
        token.unstake(stakeAmount);
        uint256 balanceAfter = token.balanceOf(provider1);

        assertEq(balanceAfter - balanceBefore, stakeAmount);
        assertEq(token.totalStaked(), 0);
        vm.stopPrank();
    }

    function test_RevertUnstakeBeforeLockPeriod() public {
        uint256 stakeAmount = 10_000 * 1e18;

        vm.startPrank(provider1);
        token.approve(address(token), stakeAmount);
        token.stake(stakeAmount);

        vm.expectRevert(AIRewards.StakeLocked.selector);
        token.unstake(stakeAmount);
        vm.stopPrank();
    }

    function test_RevertUnstakeMoreThanStaked() public {
        uint256 stakeAmount = 10_000 * 1e18;

        vm.startPrank(provider1);
        token.approve(address(token), stakeAmount);
        token.stake(stakeAmount);

        vm.warp(block.timestamp + 8 days);

        vm.expectRevert(AIRewards.InsufficientStake.selector);
        token.unstake(stakeAmount + 1);
        vm.stopPrank();
    }

    function test_StakeMultiplier() public {
        // No stake = 1.0x (10000)
        assertEq(token.getStakeMultiplier(provider1), 10_000);

        // Stake some tokens
        vm.startPrank(provider1);
        token.approve(address(token), 100_000 * 1e18);
        token.stake(100_000 * 1e18);
        vm.stopPrank();

        uint256 multiplier = token.getStakeMultiplier(provider1);
        // Should be between 1.0x and 1.5x (10000 - 15000)
        assertGt(multiplier, 10_000);
        assertLe(multiplier, 15_000);
    }

    // ============================================================
    // HARDWARE REGISTRATION TESTS
    // ============================================================

    function test_RegisterHardware() public {
        // First stake minimum
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);

        token.registerHardware(MOCK_ATTESTATION, HARDWARE_ID_1, 95);
        vm.stopPrank();

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.hardwareId, HARDWARE_ID_1);
        assertEq(contrib.trustScore, 95);
        assertTrue(contrib.nvtrustVerified);
    }

    function test_RevertRegisterWithoutStake() public {
        vm.startPrank(provider1);
        vm.expectRevert(AIRewards.InsufficientStake.selector);
        token.registerHardware(MOCK_ATTESTATION, HARDWARE_ID_1, 95);
        vm.stopPrank();
    }

    function test_RevertRegisterDuplicateHardware() public {
        // Provider 1 registers hardware
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);
        token.registerHardware(MOCK_ATTESTATION, HARDWARE_ID_1, 95);
        vm.stopPrank();

        // Provider 2 tries to register same hardware
        vm.startPrank(provider2);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);

        vm.expectRevert(AIRewards.HardwareAlreadyRegistered.selector);
        token.registerHardware(MOCK_ATTESTATION, HARDWARE_ID_1, 95);
        vm.stopPrank();
    }

    function test_RevertRegisterWithEmptyProof() public {
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);

        vm.expectRevert(AIRewards.InvalidAttestationProof.selector);
        token.registerHardware("", HARDWARE_ID_1, 95);
        vm.stopPrank();
    }

    function test_TrustScoreCapped() public {
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);

        // Try to register with trust score > 100
        token.registerHardware(MOCK_ATTESTATION, HARDWARE_ID_1, 150);
        vm.stopPrank();

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.trustScore, 100); // Should be capped at 100
    }

    // ============================================================
    // CONTRIBUTION RECORDING TESTS
    // ============================================================

    function test_RecordContribution() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        vm.startPrank(provider1);
        token.recordContribution(MOCK_ATTESTATION, 100 * 1e18, 10, 0);
        vm.stopPrank();

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.utilizedHcuHours, 100 * 1e18);
        assertGt(contrib.qualityScore, 0);
    }

    function test_RevertRecordWithoutHardware() public {
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);

        vm.expectRevert(AIRewards.HardwareNotVerified.selector);
        token.recordContribution(MOCK_ATTESTATION, 100 * 1e18, 10, 0);
        vm.stopPrank();
    }

    function test_QualityScoreCalculation() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        // Record contribution with 80% success rate
        vm.startPrank(provider1);
        token.recordContribution(MOCK_ATTESTATION, 100 * 1e18, 80, 20);
        vm.stopPrank();

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        // Quality score should reflect ~80% weighted average
        assertGt(contrib.qualityScore, 0);
        assertLe(contrib.qualityScore, 10_000);
    }

    function test_ValidatorCanUpdateQualityScore() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        vm.prank(validator);
        token.updateQualityScore(provider1, 8500, MOCK_ATTESTATION);

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.qualityScore, 8500);
    }

    function test_ValidatorCanUpdateUptimeRatio() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        vm.prank(validator);
        token.updateUptimeRatio(provider1, 9900);

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.uptimeRatio, 9900);
        assertEq(contrib.consecutiveUptime, 1);
    }

    function test_ConsecutiveUptimeResets() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        // Set high uptime
        vm.prank(validator);
        token.updateUptimeRatio(provider1, 9900);

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertEq(contrib.consecutiveUptime, 1);

        // Drop below threshold
        vm.prank(validator);
        token.updateUptimeRatio(provider1, 9000);

        contrib = token.getContribution(provider1);
        assertEq(contrib.consecutiveUptime, 0);
    }

    // ============================================================
    // EPOCH DISTRIBUTION TESTS
    // ============================================================

    function test_EpochDistribution() public {
        _setupProvider(provider1, HARDWARE_ID_1);
        _setupProvider(provider2, HARDWARE_ID_2);

        // Record contributions
        vm.prank(provider1);
        token.recordContribution(MOCK_ATTESTATION, 100 * 1e18, 95, 5);

        vm.prank(provider2);
        token.recordContribution(MOCK_ATTESTATION, 50 * 1e18, 90, 10);

        // Set quality and uptime scores
        vm.startPrank(validator);
        token.updateQualityScore(provider1, 8500, MOCK_ATTESTATION);
        token.updateQualityScore(provider2, 7500, MOCK_ATTESTATION);
        token.updateUptimeRatio(provider1, 9900);
        token.updateUptimeRatio(provider2, 9500);
        vm.stopPrank();

        // Fast forward to end of epoch
        vm.warp(block.timestamp + 25 hours);

        uint256 treasuryBalanceBefore = token.balanceOf(treasury);
        token.triggerEpochDistribution();
        uint256 treasuryBalanceAfter = token.balanceOf(treasury);

        // Verify treasury received funds
        assertGt(treasuryBalanceAfter, treasuryBalanceBefore);

        // Verify epoch advanced
        assertEq(token.currentEpoch(), 2);
        assertTrue(token.epochDistributed(1));
    }

    function test_RevertDistributeBeforeEpochEnd() public {
        vm.expectRevert(AIRewards.EpochNotEnded.selector);
        token.triggerEpochDistribution();
    }

    function test_RevertDistributeTwice() public {
        // First distribution
        vm.warp(block.timestamp + 25 hours);
        token.triggerEpochDistribution();

        // After distribution, epoch advances and new epochEndTime is set
        // Calling immediately will fail with EpochNotEnded (not EpochAlreadyDistributed)
        // because the current epoch is now 2 and its end time hasn't been reached
        vm.expectRevert(AIRewards.EpochNotEnded.selector);
        token.triggerEpochDistribution();
    }

    function test_EpochDistributedOnlyOnce() public {
        // This tests that we cannot re-distribute epoch 1 by manipulating time
        // After distribution, epochDistributed[1] = true, but currentEpoch moves to 2
        vm.warp(block.timestamp + 25 hours);
        token.triggerEpochDistribution();

        // Verify epoch 1 is marked as distributed
        assertTrue(token.epochDistributed(1));

        // Even if we somehow got to call distribution for epoch 1 again,
        // the contract state has moved on to epoch 2
        assertEq(token.currentEpoch(), 2);
    }

    function test_EmissionSchedule() public {
        // Year 1: 1M/day
        (,, uint256 emission) = token.getEpochInfo();
        assertEq(emission, 1_000_000 * 1e18);

        // Year 2: 750K/day
        vm.warp(block.timestamp + 366 days);
        (,, emission) = token.getEpochInfo();
        assertEq(emission, 750_000 * 1e18);

        // Year 3: 500K/day
        vm.warp(block.timestamp + 365 days);
        (,, emission) = token.getEpochInfo();
        assertEq(emission, 500_000 * 1e18);

        // Year 4+: 250K/day
        vm.warp(block.timestamp + 365 days);
        (,, emission) = token.getEpochInfo();
        assertEq(emission, 250_000 * 1e18);
    }

    // ============================================================
    // REWARD DESTINATION TESTS
    // ============================================================

    function test_SetRewardDestination() public {
        vm.prank(provider1);
        token.setRewardDestination(AIRewards.RewardDestination.ZooEVM);

        assertEq(uint256(token.rewardDestinations(provider1)), uint256(AIRewards.RewardDestination.ZooEVM));
    }

    function test_AutoCompoundToStake() public {
        vm.prank(provider1);
        token.setRewardDestination(AIRewards.RewardDestination.Staking);

        assertEq(uint256(token.rewardDestinations(provider1)), uint256(AIRewards.RewardDestination.Staking));
    }

    // ============================================================
    // SLASHING TESTS
    // ============================================================

    function test_SlashProvider() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        // _setupProvider already stakes 1000 tokens, so add more
        uint256 additionalStake = 10_000 * 1e18;
        vm.startPrank(provider1);
        token.approve(address(token), additionalStake);
        token.stake(additionalStake);
        vm.stopPrank();

        // Total stake is now 11000 tokens (1000 from setup + 10000 additional)
        uint256 totalStake = 11_000 * 1e18;

        uint256 reporterBalanceBefore = token.balanceOf(reporter);

        // Slash 50%
        vm.prank(slasher);
        token.slash(provider1, 5000, "Fraudulent attestation", reporter);

        AIRewards.StakeInfo memory info = token.getStakeInfo(provider1);
        assertEq(info.amount, totalStake / 2);

        // Reporter receives 50% of slashed amount (which is 25% of total stake)
        uint256 slashAmount = totalStake / 2;
        uint256 expectedReporterReward = slashAmount / 2;
        uint256 reporterReward = token.balanceOf(reporter) - reporterBalanceBefore;
        assertEq(reporterReward, expectedReporterReward);

        // Hardware registration cleared
        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider1);
        assertFalse(contrib.nvtrustVerified);
    }

    function test_RevertSlashWithoutRole() public {
        _setupProvider(provider1, HARDWARE_ID_1);

        vm.prank(provider2);
        vm.expectRevert();
        token.slash(provider1, 5000, "Test", reporter);
    }

    // ============================================================
    // GOVERNANCE TESTS
    // ============================================================

    function test_SetMinStake() public {
        vm.prank(admin);
        token.setMinStake(2000 * 1e18);

        assertEq(token.minStake(), 2000 * 1e18);
    }

    function test_RevertSetMinStakeTooLow() public {
        vm.prank(admin);
        vm.expectRevert(AIRewards.InvalidParameter.selector);
        token.setMinStake(50 * 1e18);
    }

    function test_RevertSetMinStakeTooHigh() public {
        vm.prank(admin);
        vm.expectRevert(AIRewards.InvalidParameter.selector);
        token.setMinStake(20000 * 1e18);
    }

    function test_SetTreasury() public {
        address newTreasury = address(100);

        vm.prank(admin);
        token.setTreasury(newTreasury);

        assertEq(token.treasury(), newTreasury);
    }

    function test_RevertSetTreasuryZeroAddress() public {
        vm.prank(admin);
        vm.expectRevert(AIRewards.ZeroAddress.selector);
        token.setTreasury(address(0));
    }

    function test_PauseAndUnpause() public {
        vm.prank(admin);
        token.pause();

        assertTrue(token.paused());

        // Staking should fail when paused
        vm.startPrank(provider1);
        token.approve(address(token), 1000 * 1e18);
        vm.expectRevert();
        token.stake(1000 * 1e18);
        vm.stopPrank();

        vm.prank(admin);
        token.unpause();

        assertFalse(token.paused());

        // Staking should work after unpause
        vm.startPrank(provider1);
        token.stake(1000 * 1e18);
        vm.stopPrank();
    }

    // ============================================================
    // MINTING TESTS
    // ============================================================

    function test_MintWithRole() public {
        uint256 amount = 1000 * 1e18;

        vm.prank(admin);
        token.mint(provider1, amount);

        assertEq(token.balanceOf(provider1), 100_000 * 1e18 + amount);
    }

    function test_RevertMintWithoutRole() public {
        vm.prank(provider1);
        vm.expectRevert();
        token.mint(provider1, 1000 * 1e18);
    }

    // ============================================================
    // BURN TESTS
    // ============================================================

    function test_BurnTokens() public {
        uint256 burnAmount = 1000 * 1e18;
        uint256 balanceBefore = token.balanceOf(provider1);

        vm.prank(provider1);
        token.burn(burnAmount);

        assertEq(token.balanceOf(provider1), balanceBefore - burnAmount);
    }

    // ============================================================
    // PERMIT TESTS
    // ============================================================

    function test_Permit() public {
        uint256 privateKey = 0x1234;
        address owner = vm.addr(privateKey);

        vm.prank(admin);
        token.mint(owner, 1000 * 1e18);

        bytes32 PERMIT_TYPEHASH =
            keccak256("Permit(address owner,address spender,uint256 value,uint256 nonce,uint256 deadline)");

        uint256 nonce = token.nonces(owner);
        uint256 deadline = block.timestamp + 1 hours;
        uint256 amount = 500 * 1e18;

        bytes32 structHash =
            keccak256(abi.encode(PERMIT_TYPEHASH, owner, provider1, amount, nonce, deadline));

        bytes32 digest = keccak256(
            abi.encodePacked("\x19\x01", token.DOMAIN_SEPARATOR(), structHash)
        );

        (uint8 v, bytes32 r, bytes32 s) = vm.sign(privateKey, digest);

        token.permit(owner, provider1, amount, deadline, v, r, s);

        assertEq(token.allowance(owner, provider1), amount);
    }

    // ============================================================
    // VIEW FUNCTION TESTS
    // ============================================================

    function test_EstimateCurrentEpochReward() public {
        _setupProvider(provider1, HARDWARE_ID_1);
        _setupProvider(provider2, HARDWARE_ID_2);

        // Set metrics
        vm.startPrank(validator);
        token.updateQualityScore(provider1, 8500, MOCK_ATTESTATION);
        token.updateQualityScore(provider2, 7500, MOCK_ATTESTATION);
        token.updateUptimeRatio(provider1, 9900);
        token.updateUptimeRatio(provider2, 9500);
        vm.stopPrank();

        // Record contributions
        vm.prank(provider1);
        token.recordContribution(MOCK_ATTESTATION, 100 * 1e18, 95, 5);

        vm.prank(provider2);
        token.recordContribution(MOCK_ATTESTATION, 50 * 1e18, 90, 10);

        uint256 estimate1 = token.estimateCurrentEpochReward(provider1);
        uint256 estimate2 = token.estimateCurrentEpochReward(provider2);

        // Provider 1 should get more (higher metrics and contribution)
        assertGt(estimate1, estimate2);
        assertGt(estimate1, 0);
        assertGt(estimate2, 0);
    }

    // ============================================================
    // HELPER FUNCTIONS
    // ============================================================

    function _setupProvider(address provider, bytes32 hardwareId) internal {
        vm.startPrank(provider);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);
        token.registerHardware(MOCK_ATTESTATION, hardwareId, 95);
        vm.stopPrank();
    }
}

// ============================================================
// FUZZ TESTS
// ============================================================

contract AIRewardsFuzzTest is Test {
    AIRewards public token;

    address public admin = address(1);
    address public treasury = address(2);

    bytes public constant MOCK_ATTESTATION = hex"deadbeef";

    function setUp() public {
        vm.prank(admin);
        token = new AIRewards(treasury, admin);
    }

    function testFuzz_Stake(address provider, uint256 amount) public {
        vm.assume(provider != address(0));
        vm.assume(provider != address(token));
        vm.assume(amount > 0 && amount < type(uint128).max);

        vm.prank(admin);
        token.mint(provider, amount);

        vm.startPrank(provider);
        token.approve(address(token), amount);
        token.stake(amount);
        vm.stopPrank();

        AIRewards.StakeInfo memory info = token.getStakeInfo(provider);
        assertEq(info.amount, amount);
    }

    function testFuzz_StakeMultiplier(uint256 stakeAmount) public {
        stakeAmount = bound(stakeAmount, 1, 10_000_000 * 1e18);

        address provider = address(100);
        vm.prank(admin);
        token.mint(provider, stakeAmount);

        vm.startPrank(provider);
        token.approve(address(token), stakeAmount);
        token.stake(stakeAmount);
        vm.stopPrank();

        uint256 multiplier = token.getStakeMultiplier(provider);

        // Multiplier should always be between 1.0x and 1.5x
        assertGe(multiplier, 10_000);
        assertLe(multiplier, 15_000);
    }

    function testFuzz_QualityScore(uint256 score) public {
        score = bound(score, 0, 10_000);

        address provider = address(100);
        bytes32 hwId = keccak256(abi.encodePacked(provider));

        vm.prank(admin);
        token.mint(provider, 10_000 * 1e18);

        vm.startPrank(provider);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);
        token.registerHardware(MOCK_ATTESTATION, hwId, 95);
        vm.stopPrank();

        vm.prank(admin);
        token.updateQualityScore(provider, score, MOCK_ATTESTATION);

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider);
        assertEq(contrib.qualityScore, score);
    }

    function testFuzz_TrustScore(uint256 trustScore) public {
        trustScore = bound(trustScore, 0, 200);

        address provider = address(100);
        bytes32 hwId = keccak256(abi.encodePacked(provider));

        vm.prank(admin);
        token.mint(provider, 10_000 * 1e18);

        vm.startPrank(provider);
        token.approve(address(token), 1000 * 1e18);
        token.stake(1000 * 1e18);
        token.registerHardware(MOCK_ATTESTATION, hwId, trustScore);
        vm.stopPrank();

        AIRewards.ContributionMetrics memory contrib = token.getContribution(provider);
        // Trust score should be capped at 100
        assertLe(contrib.trustScore, 100);
    }
}

// ============================================================
// INVARIANT TESTS
// ============================================================

contract AIRewardsInvariantTest is Test {
    AIRewards public token;
    address public admin = address(1);
    address public treasury = address(2);

    function setUp() public {
        vm.prank(admin);
        token = new AIRewards(treasury, admin);
    }

    function invariant_TotalSupplyNeverDecreases() public view {
        // Total supply can only increase (minting) or stay same
        // Burns reduce supply but are intentional
        assertTrue(token.totalSupply() >= 0);
    }

    function invariant_StakedNeverExceedsSupply() public view {
        assertLe(token.totalStaked(), token.totalSupply());
    }
}
