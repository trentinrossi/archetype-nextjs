import {
  AccountBalance,
  ProcessPaymentRequest,
  ProcessPaymentResponse,
  Transaction,
} from '@/types/bill-payment';

const API_BASE_URL = '/api';

class BillPaymentService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccountBalance(accountId: string): Promise<AccountBalance> {
    const response = await fetch(`${API_BASE_URL}/accounts/${accountId}/balance`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch account balance');
    }
    return response.json();
  }

  async processPayment(data: ProcessPaymentRequest): Promise<ProcessPaymentResponse> {
    const response = await fetch(`${API_BASE_URL}/accounts/process-payment`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to process payment');
    }
    return response.json();
  }

  async getTransactionById(transactionId: number): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/${transactionId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transaction');
    }
    return response.json();
  }

  async getTransactionsByAccount(accountId: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions');
    }
    return response.json();
  }
}

export const billPaymentService = new BillPaymentService();
