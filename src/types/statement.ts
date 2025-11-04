import { Customer } from './customer';
import { Account } from './account';
import { Transaction } from './transaction';

export interface InterestCalculation {
  accountId: string;
  balance: number;
  interestRate: number;
  monthlyInterest: number;
  transactionTypeCode: string;
  transactionCategoryCode: string;
}

export interface AccountStatement {
  customer: Customer;
  account: Account;
  transactions: Transaction[];
  totalAmount: number;
  statementPeriodStart?: string;
  statementPeriodEnd?: string;
  previousBalance?: number;
  currentBalance?: number;
  minimumPaymentDue?: number;
  paymentDueDate?: string;
  interestCharged?: number;
  feesCharged?: number;
  creditsApplied?: number;
  generatedAt?: string;
}

export interface CalculateInterestRequest {
  processingDate: string;
}

export interface CalculateInterestResponse {
  accountId: string;
  balance: number;
  interestRate: number;
  monthlyInterest: number;
  transactionTypeCode: string;
  transactionCategoryCode: string;
  calculatedAt?: string;
}

export interface GenerateStatementRequest {
  accountId?: string;
  cardNumber?: string;
  periodStart?: string;
  periodEnd?: string;
}

export interface StatementSummary {
  statementId?: string;
  accountId: string;
  customerId?: string;
  statementPeriodStart?: string;
  statementPeriodEnd?: string;
  totalTransactions?: number;
  totalAmount: number;
  currentBalance?: number;
  minimumPaymentDue?: number;
  paymentDueDate?: string;
  generatedAt?: string;
}
