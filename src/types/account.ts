export interface Account {
  accountId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate?: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId?: string;
  availableCredit: number;
  active: boolean;
  expired: boolean;
}

export interface AccountCreateRequest {
  accountId: string;
  activeStatus: string;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  groupId?: string;
}

export interface AccountUpdateRequest {
  activeStatus?: string;
  creditLimit?: number;
  cashCreditLimit?: number;
  expirationDate?: string;
  reissueDate?: string;
  groupId?: string;
}
