export interface UserSession {
  id: number;
  transactionId: string;
  programName: string;
  fromProgram: string;
  fromTransaction: string;
  programContext: number;
  reenterFlag: boolean;
  toProgram: string;
  programReenterFlag: string;
  userType: string;
  fromTransactionId: string;
  userId: string;
  sessionContext: string;
  isAdminUser: boolean;
  isReentering: boolean;
  isProgramReentering: boolean;
  hasCallingProgram: boolean;
  hasContext: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateUserSessionRequest {
  transactionId: string;
  programName: string;
  fromProgram?: string;
  fromTransaction?: string;
  programContext?: number;
  reenterFlag?: boolean;
  toProgram?: string;
  programReenterFlag?: string;
  userType: string;
  fromTransactionId?: string;
  userId: string;
}

export interface UpdateUserSessionRequest {
  transactionId?: string;
  programName?: string;
  fromProgram?: string;
  fromTransaction?: string;
  programContext?: number;
  reenterFlag?: boolean;
  toProgram?: string;
  programReenterFlag?: string;
  userType?: string;
  fromTransactionId?: string;
}
