// @effect-diagnostics globalDate:off -- certificate validity is wall-clock time outside an Effect runtime.
// A throwaway ECDSA P-256 certificate for a WebTransport test server, which pins it by hash.
// @effect-diagnostics-next-line nodeBuiltinImport:off -- DER encoding needs the platform's key and hash primitives.
import * as Crypto from "node:crypto";

const length = (size: number): Buffer =>
  size < 0x80
    ? Buffer.from([size])
    : size < 0x100
      ? Buffer.from([0x81, size])
      : Buffer.from([0x82, size >> 8, size & 0xff]);
const der = (tag: number, ...parts: ReadonlyArray<Buffer>): Buffer => {
  const body = Buffer.concat(parts);
  return Buffer.concat([Buffer.from([tag]), length(body.length), body]);
};
const oid = (bytes: ReadonlyArray<number>) => der(0x06, Buffer.from(bytes));
// ecdsa-with-SHA256, commonName
const ECDSA_SHA256 = der(0x30, oid([0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]));
const COMMON_NAME = oid([0x55, 0x04, 0x03]);
const utcTime = (date: Date) =>
  der(0x17, Buffer.from(`${date.toISOString().slice(2, 19).replace(/[-:T]/g, "")}Z`));

export interface SelfSignedCertificate {
  readonly cert: string;
  readonly privateKey: string;
  /** SHA-256 of the DER certificate: the WebTransport `serverCertificateHashes` value. */
  readonly hash: Uint8Array;
}

/** Valid from an hour ago for a day: WebTransport pinning allows at most 14 days. */
export function selfSignedCertificate(commonName = "localhost"): SelfSignedCertificate {
  const { privateKey, publicKey } = Crypto.generateKeyPairSync("ec", { namedCurve: "prime256v1" });
  const name = der(
    0x30,
    der(0x31, der(0x30, COMMON_NAME, der(0x0c, Buffer.from(commonName)))),
  );
  const start = new Date(Date.now() - 3_600_000);
  const end = new Date(Date.now() + 86_400_000);
  const tbs = der(
    0x30,
    der(0xa0, der(0x02, Buffer.from([2]))),
    der(0x02, Buffer.from([0x01]), Crypto.randomBytes(7)),
    ECDSA_SHA256,
    name,
    der(0x30, utcTime(start), utcTime(end)),
    name,
    publicKey.export({ type: "spki", format: "der" }),
    // subjectAltName: DNS localhost, IP 127.0.0.1
    der(
      0xa3,
      der(
        0x30,
        der(
          0x30,
          oid([0x55, 0x1d, 0x11]),
          der(
            0x04,
            der(0x30, der(0x82, Buffer.from(commonName)), der(0x87, Buffer.from([127, 0, 0, 1]))),
          ),
        ),
      ),
    ),
  );
  const signature = Crypto.sign("sha256", tbs, privateKey);
  const certificate = der(0x30, tbs, ECDSA_SHA256, der(0x03, Buffer.from([0]), signature));
  return {
    cert: `-----BEGIN CERTIFICATE-----\n${certificate.toString("base64").match(/.{1,64}/g)!.join("\n")}\n-----END CERTIFICATE-----\n`,
    privateKey: privateKey.export({ type: "pkcs8", format: "pem" }).toString(),
    hash: new Uint8Array(Crypto.createHash("sha256").update(certificate).digest()),
  };
}
