import { 
  Transaction, 
  CreateTransactionRequest 
} from '@/types/cardDemo';

const API_BASE_URL = '/api';

class TransactionService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getTransactions(): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions');
    }
    return response.json();
  }

  async getTransactionById(transactionId: string): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/${transactionId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transaction');
    }
    return response.json();
  }

  async getTransactionsByCardNumber(cardNumber: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/card/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions by card number');
    }
    return response.json();
  }

  async getTransactionsByAccountId(accountId: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/transactions/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transactions by account ID');
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
      throw new Error('Failed to create transaction');
    }
    return response.json();
  }
}

export const transactionService = new TransactionService();
