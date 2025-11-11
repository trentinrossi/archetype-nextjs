export interface UserSession {
  id: number;
  transactionId: string;
  programName: string;
  fromProgram?: string;
  fromTransaction?: string;
  programContext?: number;
  reenterFlag: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateUserSessionRequest {
  transactionId: string;
  programName: string;
  fromProgram?: string;
  fromTransaction?: string;
  programContext?: number;
  reenterFlag: boolean;
}

export interface UpdateUserSessionRequest {
  programName?: string;
  reenterFlag?: boolean;
}
