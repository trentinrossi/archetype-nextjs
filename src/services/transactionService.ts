import { 
  Transaction, 
  CreateTransactionRequest,
  PaginatedTransactions 
} from '@/types/transaction';

const API_BASE_URL = '/api';

class TransactionService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getTransactions(page: number = 0, size: number = 20, sort?: string): Promise<PaginatedTransactions> {
    let url = `${API_BASE_URL}/transactions?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions');
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
      throw new Error('Failed to fetch transactions by account');
    }
    return response.json();
  }

  async getTransactionsByCard(cardNumber: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/card/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions by card');
    }
    return response.json();
  }

  async getBillPaymentTransactions(): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/bill-payments`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch bill payment transactions');
    }
    return response.json();
  }

  async getBillPaymentTransactionsByAccount(accountId: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/bill-payments/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch bill payment transactions by account');
    }
    return response.json();
  }

  async createTransaction(data: CreateTransactionRequest): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to create transaction');
    }
    return response.json();
  }

  async deleteTransaction(transactionId: number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/transactions/${transactionId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete transaction');
    }
  }
}

export const transactionService = new TransactionService();
