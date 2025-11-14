export interface Account {
  accountId: number;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId?: string;
  customerId: number;
  accountStatus: string;
  availableCredit?: number;
  availableCashCredit?: number;
  netCycleAmount?: number;
  isActive?: boolean;
  isExpired?: boolean;
  hasGroupAssignment?: boolean;
  activeStatusDisplay?: string;
  accountStatusDisplay?: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateAccountRequest {
  accountId: number;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId?: string;
  customerId: number;
  accountStatus: string;
}

export interface UpdateAccountRequest {
  activeStatus?: string;
  currentBalance?: number;
  creditLimit?: number;
  cashCreditLimit?: number;
  openDate?: string;
  expirationDate?: string;
  reissueDate?: string;
  currentCycleCredit?: number;
  currentCycleDebit?: number;
  groupId?: string;
  customerId?: number;
  accountStatus?: string;
}
