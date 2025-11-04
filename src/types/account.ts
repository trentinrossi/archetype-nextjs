export interface Account {
  accountId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  addressZipCode: string;
  groupId: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateAccountRequest {
  accountId: string;
  activeStatus: string;
  currentBalance: number;
  creditLimit: number;
  cashCreditLimit: number;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currentCycleCredit: number;
  currentCycleDebit: number;
  addressZipCode: string;
  groupId: string;
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
  addressZipCode?: string;
  groupId?: string;
}
