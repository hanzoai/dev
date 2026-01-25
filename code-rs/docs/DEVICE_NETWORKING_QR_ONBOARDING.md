# Device Networking with QR Code Onboarding for Hanzo Node

## Executive Summary

This document describes the architecture for secure device onboarding and network discovery in the Hanzo Node ecosystem. The design leverages QR codes for initial pairing, W3C DIDs for identity, libp2p for peer discovery, and post-quantum cryptography for future-proof security.

## 1. Requirements Analysis

### 1.1 Functional Requirements

1. **Device Onboarding**: New devices must be able to join the Hanzo network through a simple QR code scan
2. **Secure Pairing**: Establish cryptographically secure channels between devices
3. **Network Discovery**: Devices should automatically discover and connect to nearby peers
4. **Identity Binding**: Link device identities to W3C DIDs for cross-chain identity verification
5. **Multi-Device Management**: Users can manage multiple devices under a single identity

### 1.2 Non-Functional Requirements

1. **Security**: Post-quantum resistant cryptography (ML-KEM, ML-DSA)
2. **Usability**: One-scan onboarding, minimal user interaction
3. **Offline-First**: Initial pairing should work without internet connectivity
4. **Performance**: Sub-second QR generation and parsing
5. **Interoperability**: Compatible with existing libp2p infrastructure

### 1.3 Security Constraints

- No long-lived secrets in QR codes
- Time-limited pairing windows (5 minutes default)
- Mutual authentication required
- PQ-hybrid key exchange for forward secrecy

## 2. Architecture Overview

```
+-------------------+     QR Code      +-------------------+
|                   |   ----------->   |                   |
|  Controller       |   Pairing Info   |  New Device       |
|  (Phone/Desktop)  |                  |  (Node/IoT)       |
|                   |   <-----------   |                   |
|                   |   Verification   |                   |
+-------------------+                  +-------------------+
        |                                      |
        |  libp2p                              |  libp2p
        |  (mDNS/DHT)                          |  (mDNS/DHT)
        v                                      v
+-----------------------------------------------------------+
|                    Hanzo P2P Network                       |
|  (Relay Nodes, Registry, Identity Resolution)             |
+-----------------------------------------------------------+
```

## 3. QR Code Generation

### 3.1 QR Payload Structure

The QR code contains a compact, URL-safe payload that encodes pairing information.

```rust
/// QR code pairing payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QRPairingPayload {
    /// Protocol version
    pub version: u8,

    /// Pairing type
    pub pairing_type: PairingType,

    /// Controller's DID (did:hanzo:username or did:lux:username)
    pub controller_did: String,

    /// Ephemeral X25519 public key for initial key exchange (32 bytes, base64url)
    pub ephemeral_pk: String,

    /// ML-KEM-768 encapsulation key (1184 bytes, base64url) for PQ hybrid
    pub mlkem_pk: Option<String>,

    /// Pairing challenge nonce (16 bytes, base64url)
    pub nonce: String,

    /// Timestamp (Unix seconds)
    pub timestamp: u64,

    /// Expiry duration (seconds, default 300)
    pub ttl: u16,

    /// Signature over payload (ML-DSA-65 or Ed25519, base64url)
    pub signature: String,

    /// Network hints for peer discovery
    pub network_hints: Option<NetworkHints>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PairingType {
    /// New device being added to user's identity
    DeviceOnboarding,
    /// Temporary session pairing (e.g., kiosk access)
    SessionPairing,
    /// Device-to-device direct connection
    DirectConnect,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkHints {
    /// Local IP addresses if on same network
    pub local_addrs: Vec<String>,
    /// Relay node multiaddresses
    pub relay_addrs: Vec<String>,
    /// libp2p peer ID
    pub peer_id: Option<String>,
}
```

### 3.2 QR Code Format

The QR payload is encoded as a compact URL for maximum compatibility:

```
hanzo://pair/v1/{base64url_encoded_payload}
```

For smaller QR codes (essential for device screens), we use a compressed binary format:

```rust
/// Compact binary QR format for small displays
pub struct CompactQRPayload {
    /// Version + pairing type (1 byte: version << 4 | type)
    pub header: u8,

    /// Ephemeral X25519 public key (32 bytes)
    pub ephemeral_pk: [u8; 32],

    /// Nonce (16 bytes)
    pub nonce: [u8; 16],

    /// Timestamp (4 bytes, seconds since 2024-01-01)
    pub timestamp: u32,

    /// TTL (2 bytes)
    pub ttl: u16,

    /// Truncated controller ID hash (8 bytes)
    pub controller_hash: [u8; 8],

    /// Signature (64 bytes Ed25519 or 80 bytes ML-DSA compressed)
    pub signature: Vec<u8>,
}
// Total: ~130-150 bytes = easily fits in QR Code Version 5
```

### 3.3 QR Generation Implementation

```rust
use qrcode::{QrCode, EcLevel};
use image::Luma;

pub struct QRGenerator {
    controller_did: DID,
    identity_key: SigningKey,
    encryption_key: EncryptionStaticKey,
}

impl QRGenerator {
    /// Generate a pairing QR code
    pub fn generate_pairing_qr(
        &self,
        pairing_type: PairingType,
        ttl_seconds: Option<u16>,
    ) -> Result<QRPairingPayload, QRError> {
        // Generate ephemeral keys for this pairing session
        let ephemeral_sk = x25519_dalek::StaticSecret::random_from_rng(&mut OsRng);
        let ephemeral_pk = x25519_dalek::PublicKey::from(&ephemeral_sk);

        // Generate ML-KEM keypair for PQ protection
        let (mlkem_pk, mlkem_sk) = ml_kem::kem_keygen_768();

        // Generate challenge nonce
        let mut nonce = [0u8; 16];
        OsRng.fill_bytes(&mut nonce);

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let ttl = ttl_seconds.unwrap_or(300);

        // Build payload for signing
        let mut payload_bytes = Vec::new();
        payload_bytes.extend_from_slice(&[1u8]); // version
        payload_bytes.extend_from_slice(&(pairing_type as u8).to_le_bytes());
        payload_bytes.extend_from_slice(self.controller_did.to_string().as_bytes());
        payload_bytes.extend_from_slice(ephemeral_pk.as_bytes());
        payload_bytes.extend_from_slice(&nonce);
        payload_bytes.extend_from_slice(&timestamp.to_le_bytes());
        payload_bytes.extend_from_slice(&ttl.to_le_bytes());

        // Sign with ML-DSA or Ed25519 (hybrid approach)
        let signature = self.sign_payload(&payload_bytes)?;

        Ok(QRPairingPayload {
            version: 1,
            pairing_type,
            controller_did: self.controller_did.to_string(),
            ephemeral_pk: base64_url::encode(ephemeral_pk.as_bytes()),
            mlkem_pk: Some(base64_url::encode(&mlkem_pk.as_bytes())),
            nonce: base64_url::encode(&nonce),
            timestamp,
            ttl,
            signature: base64_url::encode(&signature),
            network_hints: self.get_network_hints(),
        })
    }

    /// Render QR code to image
    pub fn render_qr(&self, payload: &QRPairingPayload) -> Result<Vec<u8>, QRError> {
        let url = format!(
            "hanzo://pair/v1/{}",
            base64_url::encode(&serde_json::to_vec(payload)?)
        );

        let code = QrCode::with_error_correction_level(url.as_bytes(), EcLevel::M)?;
        let image = code.render::<Luma<u8>>()
            .min_dimensions(200, 200)
            .build();

        let mut png_bytes = Vec::new();
        image::DynamicImage::ImageLuma8(image)
            .write_to(&mut Cursor::new(&mut png_bytes), image::ImageFormat::Png)?;

        Ok(png_bytes)
    }
}
```

## 4. Secure Pairing Protocol

### 4.1 Protocol Flow

```
Controller                                         New Device
    |                                                   |
    |--- 1. Generate QR (ephemeral_pk, mlkem_pk, sig) ->|
    |                                                   |
    |                          [User scans QR]          |
    |                                                   |
    |<- 2. PairingRequest(device_pk, device_mlkem_pk) --|
    |                                                   |
    |   3. [Verify signature, check timestamp]          |
    |   4. [Compute shared secret: ECDH + ML-KEM]       |
    |                                                   |
    |-- 5. PairingChallenge(encrypted_challenge) ------>|
    |                                                   |
    |               6. [Decrypt challenge]              |
    |               7. [Sign challenge with device key] |
    |                                                   |
    |<- 8. PairingResponse(signed_challenge, device_info)|
    |                                                   |
    |   9. [Verify device signature]                    |
    |  10. [Register device in identity]                |
    |                                                   |
    |-- 11. PairingConfirm(encrypted_credentials) ----->|
    |                                                   |
    |              12. [Store credentials]              |
    |              13. [Join network]                   |
    |                                                   |
    |<-------------- Secure Channel Established ------->|
```

### 4.2 Pairing Messages

```rust
/// Pairing request from new device
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingRequest {
    /// New device's ephemeral X25519 public key
    pub device_ephemeral_pk: [u8; 32],

    /// New device's ML-KEM public key for PQ hybrid
    pub device_mlkem_pk: Vec<u8>,

    /// New device's signing public key (for future authentication)
    pub device_signing_pk: Vec<u8>,

    /// Echo of nonce from QR code
    pub nonce: [u8; 16],

    /// Device metadata
    pub device_info: DeviceInfo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceInfo {
    /// Human-readable device name
    pub name: String,
    /// Device type
    pub device_type: DeviceType,
    /// Device capabilities
    pub capabilities: Vec<String>,
    /// Hardware attestation (if available)
    pub attestation: Option<AttestationBundle>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DeviceType {
    Desktop,
    Mobile,
    IoT,
    Server,
    Embedded,
}

/// Challenge from controller
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingChallenge {
    /// Encrypted using hybrid shared secret
    pub encrypted_challenge: Vec<u8>,
    /// AEAD nonce
    pub nonce: [u8; 12],
}

/// Response from device
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingResponse {
    /// Signed challenge response
    pub signed_challenge: Vec<u8>,
    /// Device signature over pairing transcript
    pub signature: Vec<u8>,
}

/// Confirmation from controller
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PairingConfirm {
    /// Encrypted device credentials
    pub encrypted_credentials: Vec<u8>,
    /// AEAD nonce
    pub nonce: [u8; 12],
    /// Device's assigned DID
    pub device_did: String,
    /// Network bootstrap information
    pub bootstrap_info: BootstrapInfo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BootstrapInfo {
    /// Relay nodes to connect to
    pub relay_addrs: Vec<String>,
    /// DHT bootstrap peers
    pub bootstrap_peers: Vec<String>,
    /// Registry contract address
    pub registry_address: String,
    /// RPC endpoints
    pub rpc_endpoints: Vec<String>,
}
```

### 4.3 Key Exchange Implementation

```rust
/// Hybrid key exchange combining X25519 and ML-KEM-768
pub struct HybridKeyExchange {
    x25519_sk: x25519_dalek::StaticSecret,
    x25519_pk: x25519_dalek::PublicKey,
    mlkem_dk: ml_kem::DecapsKey768,
    mlkem_ek: ml_kem::EncapsKey768,
}

impl HybridKeyExchange {
    pub fn new() -> Self {
        let x25519_sk = x25519_dalek::StaticSecret::random_from_rng(&mut OsRng);
        let x25519_pk = x25519_dalek::PublicKey::from(&x25519_sk);
        let (mlkem_ek, mlkem_dk) = ml_kem::kem_keygen_768();

        Self {
            x25519_sk,
            x25519_pk,
            mlkem_dk,
            mlkem_ek,
        }
    }

    /// Derive shared secret from peer's public keys
    pub fn derive_shared_secret(
        &self,
        peer_x25519_pk: &[u8; 32],
        peer_mlkem_ciphertext: &[u8],
    ) -> Result<[u8; 32], KeyExchangeError> {
        // X25519 shared secret
        let peer_pk = x25519_dalek::PublicKey::from(*peer_x25519_pk);
        let x25519_shared = self.x25519_sk.diffie_hellman(&peer_pk);

        // ML-KEM decapsulation
        let mlkem_shared = self.mlkem_dk.decapsulate(peer_mlkem_ciphertext)?;

        // Combine both shared secrets using HKDF
        let combined_secret = hkdf_combine(
            x25519_shared.as_bytes(),
            mlkem_shared.as_bytes(),
            b"hanzo-pairing-v1",
        );

        Ok(combined_secret)
    }

    /// Encapsulate to peer's ML-KEM public key
    pub fn encapsulate_to_peer(
        &self,
        peer_mlkem_pk: &[u8],
    ) -> Result<(Vec<u8>, [u8; 32]), KeyExchangeError> {
        let (ciphertext, shared_secret) = ml_kem::encapsulate_768(peer_mlkem_pk)?;
        Ok((ciphertext, shared_secret))
    }
}

/// HKDF combination of multiple shared secrets
fn hkdf_combine(secret1: &[u8], secret2: &[u8], info: &[u8]) -> [u8; 32] {
    use hkdf::Hkdf;
    use sha2::Sha384;

    let mut combined_ikm = Vec::with_capacity(secret1.len() + secret2.len());
    combined_ikm.extend_from_slice(secret1);
    combined_ikm.extend_from_slice(secret2);

    let hk = Hkdf::<Sha384>::new(None, &combined_ikm);
    let mut output = [0u8; 32];
    hk.expand(info, &mut output).expect("32 bytes is valid");
    output
}
```

## 5. Network Discovery Mechanism

### 5.1 Discovery Layers

The network discovery system operates on multiple layers for resilience:

```
Layer 1: Local Network (mDNS)
    - Discovers devices on the same LAN
    - Zero configuration required
    - Works offline

Layer 2: Direct Connection (Network Hints from QR)
    - Uses IP addresses from QR code
    - Works when devices are on same network
    - Immediate connection attempt

Layer 3: Relay Assisted (libp2p Relays)
    - Uses Hanzo relay infrastructure
    - Works across NATs and firewalls
    - DCUtR for hole punching when possible

Layer 4: DHT Discovery (Kademlia)
    - Global peer discovery
    - Identity-based routing via DID
    - Fallback for unknown networks
```

### 5.2 Discovery Implementation

```rust
use libp2p::{
    identify,
    kad,
    mdns,
    relay,
    dcutr,
    gossipsub,
    request_response,
    swarm::{NetworkBehaviour, SwarmBuilder},
    PeerId,
    Multiaddr,
};

/// Combined network behaviour for device discovery
#[derive(NetworkBehaviour)]
pub struct HanzoDiscoveryBehaviour {
    /// Local network discovery
    mdns: mdns::tokio::Behaviour,

    /// Distributed hash table for global discovery
    kademlia: kad::Behaviour<kad::store::MemoryStore>,

    /// Relay client for NAT traversal
    relay_client: relay::client::Behaviour,

    /// Direct connection upgrade
    dcutr: dcutr::Behaviour,

    /// Peer identification
    identify: identify::Behaviour,

    /// Pairing protocol
    pairing: request_response::json::Behaviour<PairingRequest, PairingResponse>,

    /// Topic-based messaging
    gossipsub: gossipsub::Behaviour,
}

impl HanzoDiscoveryBehaviour {
    pub async fn new(
        local_key: &libp2p::identity::Keypair,
        relay_addrs: Vec<Multiaddr>,
    ) -> Result<Self, DiscoveryError> {
        let peer_id = PeerId::from(local_key.public());

        // mDNS for local discovery
        let mdns = mdns::tokio::Behaviour::new(
            mdns::Config::default(),
            peer_id,
        )?;

        // Kademlia DHT
        let store = kad::store::MemoryStore::new(peer_id);
        let mut kademlia = kad::Behaviour::new(peer_id, store);

        // Add bootstrap peers
        for addr in &relay_addrs {
            if let Some(peer_id) = extract_peer_id(addr) {
                kademlia.add_address(&peer_id, addr.clone());
            }
        }

        // Relay client
        let relay_client = relay::client::Behaviour::new(
            local_key.public().to_peer_id(),
            Default::default(),
        );

        // DCUtR for hole punching
        let dcutr = dcutr::Behaviour::new(local_key.public().to_peer_id());

        // Identify protocol
        let identify = identify::Behaviour::new(identify::Config::new(
            "/hanzo/id/1.0.0".to_string(),
            local_key.public(),
        ));

        // Pairing protocol
        let pairing = request_response::json::Behaviour::new(
            [(StreamProtocol::new("/hanzo/pairing/1.0.0"), ProtocolSupport::Full)],
            request_response::Config::default(),
        );

        // Gossipsub for topic messaging
        let gossipsub = gossipsub::Behaviour::new(
            gossipsub::MessageAuthenticity::Signed(local_key.clone()),
            gossipsub::Config::default(),
        )?;

        Ok(Self {
            mdns,
            kademlia,
            relay_client,
            dcutr,
            identify,
            pairing,
            gossipsub,
        })
    }
}

/// Peer discovery service
pub struct DiscoveryService {
    swarm: Swarm<HanzoDiscoveryBehaviour>,
    discovered_peers: Arc<RwLock<HashMap<PeerId, PeerInfo>>>,
    pending_pairings: Arc<RwLock<HashMap<[u8; 16], PendingPairing>>>,
}

#[derive(Debug, Clone)]
pub struct PeerInfo {
    pub peer_id: PeerId,
    pub did: Option<DID>,
    pub addresses: Vec<Multiaddr>,
    pub last_seen: Instant,
    pub connection_status: ConnectionStatus,
}

#[derive(Debug, Clone)]
pub enum ConnectionStatus {
    Discovered,
    Connecting,
    Connected,
    Paired,
    Disconnected,
}

impl DiscoveryService {
    /// Start discovery with network hints from QR code
    pub async fn start_with_hints(
        &mut self,
        hints: &NetworkHints,
    ) -> Result<(), DiscoveryError> {
        // Try direct connections first
        for addr in &hints.local_addrs {
            if let Ok(multiaddr) = addr.parse::<Multiaddr>() {
                self.swarm.dial(multiaddr)?;
            }
        }

        // Try relay connections
        for relay_addr in &hints.relay_addrs {
            if let Ok(multiaddr) = relay_addr.parse::<Multiaddr>() {
                self.swarm.dial(multiaddr)?;
            }
        }

        // If we have peer_id, try DHT lookup
        if let Some(peer_id_str) = &hints.peer_id {
            if let Ok(peer_id) = peer_id_str.parse::<PeerId>() {
                self.swarm.behaviour_mut().kademlia.get_closest_peers(peer_id);
            }
        }

        Ok(())
    }

    /// Resolve DID to peer addresses
    pub async fn resolve_did(&mut self, did: &DID) -> Result<Vec<Multiaddr>, DiscoveryError> {
        // Try local cache first
        if let Some(peer_info) = self.find_peer_by_did(did).await {
            return Ok(peer_info.addresses);
        }

        // Query DHT with DID as key
        let key = did_to_kad_key(did);
        self.swarm.behaviour_mut().kademlia.get_record(key);

        // Wait for response (with timeout)
        // In practice, this would be event-driven
        tokio::time::timeout(
            Duration::from_secs(10),
            self.wait_for_did_resolution(did),
        ).await?
    }

    /// Announce our DID to the network
    pub async fn announce_did(&mut self, did: &DID) -> Result<(), DiscoveryError> {
        let key = did_to_kad_key(did);
        let local_addrs: Vec<u8> = self.swarm
            .external_addresses()
            .map(|addr| addr.as_ref().to_vec())
            .flatten()
            .collect();

        let record = kad::Record::new(key, local_addrs);
        self.swarm.behaviour_mut().kademlia.put_record(record, kad::Quorum::One)?;

        Ok(())
    }
}

/// Convert DID to Kademlia key
fn did_to_kad_key(did: &DID) -> kad::RecordKey {
    use sha2::{Sha256, Digest};
    let mut hasher = Sha256::new();
    hasher.update(b"hanzo-did-v1:");
    hasher.update(did.to_string().as_bytes());
    kad::RecordKey::new(&hasher.finalize())
}
```

## 6. Device Identity Management

### 6.1 Device DID Structure

Each device gets its own DID derived from the user's root identity:

```
User Root DID:     did:hanzo:zeekay
Device 1 DID:      did:hanzo:zeekay/device/desktop-main
Device 2 DID:      did:hanzo:zeekay/device/mobile-iphone
Device 3 DID:      did:hanzo:zeekay/device/iot-sensor-1
```

### 6.2 Device Registry

```rust
/// Device registry for managing multiple devices under one identity
pub struct DeviceRegistry {
    /// Root DID for the user
    root_did: DID,

    /// Registered devices
    devices: HashMap<String, DeviceRecord>,

    /// Database handle
    db: Arc<SqliteDb>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceRecord {
    /// Device DID
    pub did: DID,

    /// Device name
    pub name: String,

    /// Device type
    pub device_type: DeviceType,

    /// Device's signing public key
    pub signing_pk: Vec<u8>,

    /// Device's encryption public key
    pub encryption_pk: Vec<u8>,

    /// Registration timestamp
    pub registered_at: u64,

    /// Last seen timestamp
    pub last_seen: u64,

    /// Device status
    pub status: DeviceStatus,

    /// Capabilities granted to device
    pub capabilities: DeviceCapabilities,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DeviceStatus {
    Active,
    Inactive,
    Revoked,
    PendingApproval,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceCapabilities {
    /// Can sign messages on behalf of user
    pub can_sign: bool,
    /// Can access all inboxes
    pub full_inbox_access: bool,
    /// Can modify settings
    pub can_modify_settings: bool,
    /// Can add new devices
    pub can_add_devices: bool,
    /// Specific tools the device can use
    pub allowed_tools: Vec<String>,
}

impl DeviceRegistry {
    /// Register a new device after successful pairing
    pub async fn register_device(
        &mut self,
        device_info: &DeviceInfo,
        signing_pk: &[u8],
        encryption_pk: &[u8],
    ) -> Result<DeviceRecord, RegistryError> {
        // Generate device DID
        let device_id = self.generate_device_id(&device_info.name);
        let device_did = DID::new(
            &self.root_did.method,
            format!("{}/device/{}", self.root_did.id, device_id),
        );

        let record = DeviceRecord {
            did: device_did.clone(),
            name: device_info.name.clone(),
            device_type: device_info.device_type.clone(),
            signing_pk: signing_pk.to_vec(),
            encryption_pk: encryption_pk.to_vec(),
            registered_at: current_timestamp(),
            last_seen: current_timestamp(),
            status: DeviceStatus::Active,
            capabilities: self.default_capabilities(&device_info.device_type),
        };

        // Store in database
        self.db.insert_device(&record).await?;

        // Announce to network
        self.announce_device(&device_did).await?;

        self.devices.insert(device_id, record.clone());

        Ok(record)
    }

    /// Revoke a device
    pub async fn revoke_device(&mut self, device_id: &str) -> Result<(), RegistryError> {
        if let Some(device) = self.devices.get_mut(device_id) {
            device.status = DeviceStatus::Revoked;
            self.db.update_device_status(device_id, DeviceStatus::Revoked).await?;

            // Broadcast revocation
            self.broadcast_revocation(&device.did).await?;
        }
        Ok(())
    }

    /// Verify a device signature
    pub fn verify_device_signature(
        &self,
        device_did: &DID,
        message: &[u8],
        signature: &[u8],
    ) -> Result<bool, RegistryError> {
        let device_id = self.extract_device_id(device_did)?;

        if let Some(device) = self.devices.get(&device_id) {
            if device.status != DeviceStatus::Active {
                return Err(RegistryError::DeviceRevoked);
            }

            // Verify signature with device's public key
            let verifying_key = VerifyingKey::from_bytes(
                device.signing_pk.as_slice().try_into()?
            )?;

            let sig = Signature::from_bytes(signature.try_into()?);
            Ok(verifying_key.verify(message, &sig).is_ok())
        } else {
            Err(RegistryError::DeviceNotFound)
        }
    }
}
```

## 7. Security Considerations

### 7.1 Threat Model

| Threat | Mitigation |
|--------|------------|
| QR code interception | Time-limited validity, ephemeral keys, signature verification |
| Man-in-the-middle | Mutual authentication, PQ hybrid key exchange |
| Replay attacks | Nonce verification, timestamp validation |
| Compromised device | Device revocation, capability restrictions |
| Quantum attacks | ML-KEM-768 + ML-DSA-65 for future-proofing |
| Physical QR exposure | Short TTL (5 min default), can regenerate |

### 7.2 Security Properties

1. **Forward Secrecy**: Each pairing uses ephemeral keys; compromising long-term keys doesn't compromise past sessions
2. **Post-Quantum Security**: Hybrid scheme provides security against both classical and quantum attackers
3. **Mutual Authentication**: Both parties verify each other's identities
4. **Binding to Identity**: Device keys are bound to user's DID, enabling revocation
5. **Least Privilege**: Devices get minimal capabilities by default

### 7.3 Key Rotation

```rust
/// Key rotation manager for devices
impl DeviceRegistry {
    /// Rotate device keys (should be done periodically)
    pub async fn rotate_device_keys(
        &mut self,
        device_id: &str,
        new_signing_pk: &[u8],
        new_encryption_pk: &[u8],
        rotation_proof: &[u8], // Signed by old key
    ) -> Result<(), RegistryError> {
        let device = self.devices.get(device_id)
            .ok_or(RegistryError::DeviceNotFound)?;

        // Verify rotation is signed by old key
        self.verify_device_signature(&device.did, new_signing_pk, rotation_proof)?;

        // Update keys
        self.db.update_device_keys(device_id, new_signing_pk, new_encryption_pk).await?;

        // Update local cache
        if let Some(device) = self.devices.get_mut(device_id) {
            device.signing_pk = new_signing_pk.to_vec();
            device.encryption_pk = new_encryption_pk.to_vec();
        }

        // Announce key rotation
        self.announce_key_rotation(device_id).await?;

        Ok(())
    }
}
```

## 8. Implementation Plan

### Phase 1: Core Pairing (Week 1-2)

1. QR code generation and parsing
2. Basic X25519 key exchange
3. Pairing protocol messages
4. Local storage of paired devices

### Phase 2: Network Discovery (Week 3-4)

1. mDNS integration for local discovery
2. Relay connection support
3. DHT-based global discovery
4. DID resolution

### Phase 3: Security Hardening (Week 5-6)

1. ML-KEM integration for PQ protection
2. Device capability system
3. Key rotation protocol
4. Revocation broadcasting

### Phase 4: Integration (Week 7-8)

1. Integration with existing Hanzo Node
2. Mobile app pairing support
3. CLI tools for device management
4. Testing and documentation

## 9. API Reference

### 9.1 QR Generation API

```rust
/// Generate a pairing QR code
pub async fn generate_pairing_qr(
    controller: &HanzoNode,
    pairing_type: PairingType,
    options: QROptions,
) -> Result<QRCode, Error>;

/// Options for QR generation
pub struct QROptions {
    pub ttl_seconds: u16,
    pub include_mlkem: bool,
    pub include_network_hints: bool,
    pub image_size: u32,
}
```

### 9.2 Pairing API

```rust
/// Initiate pairing from scanned QR
pub async fn initiate_pairing(
    device: &HanzoNode,
    qr_payload: &QRPairingPayload,
) -> Result<PairingSession, Error>;

/// Accept pairing request (controller side)
pub async fn accept_pairing(
    controller: &HanzoNode,
    request: &PairingRequest,
) -> Result<PairingConfirm, Error>;

/// Complete pairing (device side)
pub async fn complete_pairing(
    device: &HanzoNode,
    confirm: &PairingConfirm,
) -> Result<DeviceCredentials, Error>;
```

### 9.3 Discovery API

```rust
/// Start network discovery
pub async fn start_discovery(
    node: &HanzoNode,
    hints: Option<NetworkHints>,
) -> Result<DiscoveryHandle, Error>;

/// Resolve a DID to network addresses
pub async fn resolve_did(
    discovery: &DiscoveryHandle,
    did: &DID,
) -> Result<Vec<Multiaddr>, Error>;

/// Announce our presence to the network
pub async fn announce(
    discovery: &DiscoveryHandle,
) -> Result<(), Error>;
```

## 10. Testing Strategy

### 10.1 Unit Tests

- QR payload encoding/decoding
- Cryptographic operations
- Pairing message serialization
- DID parsing and formatting

### 10.2 Integration Tests

- Full pairing flow (controller + device)
- Network discovery across scenarios
- Relay-mediated connections
- Multi-device management

### 10.3 Security Tests

- Replay attack resistance
- Invalid signature rejection
- Expired QR rejection
- Revoked device handling

## 11. Dependencies

```toml
[dependencies]
# QR Code
qrcode = "0.14"
image = "0.25"

# Cryptography
x25519-dalek = "2.0"
ed25519-dalek = "2.1"
ml-kem = "0.3.0-pre"  # Post-quantum KEM
ml-dsa = "0.1.0-rc.0"  # Post-quantum signatures
chacha20poly1305 = "0.10"
hkdf = "0.12"
sha2 = "0.10"

# Networking
libp2p = { version = "0.54", features = [
    "tokio",
    "noise",
    "yamux",
    "tcp",
    "dns",
    "mdns",
    "kad",
    "relay",
    "dcutr",
    "request-response",
    "gossipsub",
    "identify",
]}

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
base64-url = "2.0"

# Async
tokio = { version = "1.41", features = ["full"] }

# Storage
sqlx = { version = "0.8", features = ["sqlite"] }
```

## 12. Conclusion

This design provides a secure, user-friendly device onboarding system that:

1. Enables one-scan QR code pairing
2. Uses post-quantum cryptography for future-proof security
3. Integrates with the existing W3C DID identity system
4. Leverages libp2p for robust network discovery
5. Supports multi-device management under a single identity

The architecture is designed to be:
- **Simple**: Minimal user interaction required
- **Secure**: Multiple layers of cryptographic protection
- **Flexible**: Works across LANs, relays, and global DHT
- **Maintainable**: Clear separation of concerns
- **Extensible**: Easy to add new device types and capabilities

---

## Appendix A: QR Code Size Considerations

| Payload Size | QR Version | Modules | Recommended For |
|--------------|------------|---------|-----------------|
| < 50 bytes   | 2          | 25x25   | Small displays |
| < 150 bytes  | 5          | 37x37   | Standard devices |
| < 300 bytes  | 8          | 49x49   | Full features |
| < 600 bytes  | 12         | 65x65   | Maximum payload |

For most use cases, Version 5 (37x37) with medium error correction provides the best balance of capacity and scannability.

## Appendix B: Privacy Tiers Compatibility

The pairing system integrates with Hanzo's privacy tiers:

| Privacy Tier | QR Contents | Network Hints | Bootstrap |
|--------------|-------------|---------------|-----------|
| T0 (Public)  | Full DID, addresses | Yes | Public relays |
| T1 (Standard)| Hashed DID | Local only | Private relays |
| T2 (Confidential)| Encrypted | No | TEE relays only |
| T3 (Secret)  | One-time pad | No | Direct only |
