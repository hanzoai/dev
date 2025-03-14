import { hanzo } from "./open-hands-axios";

class InvariantService {
  static async getPolicy() {
    const { data } = await hanzo.get("/api/security/policy");
    return data.policy;
  }

  static async getRiskSeverity() {
    const { data } = await hanzo.get("/api/security/settings");
    return data.RISK_SEVERITY;
  }

  static async getTraces() {
    const { data } = await hanzo.get("/api/security/export-trace");
    return data;
  }

  static async updatePolicy(policy: string) {
    await hanzo.post("/api/security/policy", { policy });
  }

  static async updateRiskSeverity(riskSeverity: number) {
    await hanzo.post("/api/security/settings", {
      RISK_SEVERITY: riskSeverity,
    });
  }
}

export default InvariantService;
