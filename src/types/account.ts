export interface Account {
  acctId: string;
  acctActiveStatus: 'A' | 'I';
  acctCurrBal: number;
  acctCreditLimit: number;
  acctCashCreditLimit: number;
  acctOpenDate: string;
  acctExpirationDate: string;
  acctReissueDate: string | null;
  acctCurrCycCredit: number;
  acctCurrCycDebit: number;
  acctGroupId: string | null;
  availableCredit: number;
  availableCashCredit: number;
  currentCycleNetAmount: number;
  isActive: boolean;
  isExpired: boolean;
  hasBeenReissued: boolean;
  activeStatusDisplayName: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAccountRequest {
  acctId: string;
  acctActiveStatus: 'A' | 'I';
  acctCurrBal: number;
  acctCreditLimit: number;
  acctCashCreditLimit: number;
  acctOpenDate: string;
  acctExpirationDate: string;
  acctReissueDate?: string;
  acctCurrCycCredit: number;
  acctCurrCycDebit: number;
  acctGroupId?: string;
}

export interface UpdateAccountRequest {
  acctId: string;
  acctActiveStatus?: 'A' | 'I';
  acctCurrBal?: number;
  acctCreditLimit?: number;
  acctCashCreditLimit?: number;
  acctOpenDate?: string;
  acctExpirationDate?: string;
  acctReissueDate?: string;
  acctCurrCycCredit?: number;
  acctCurrCycDebit?: number;
  acctGroupId?: string;
}

export interface AccountsPageResponse {
  content: Account[];
  pageable: {
    pageNumber: number;
    pageSize: number;
  };
  totalElements: number;
  totalPages: number;
}
