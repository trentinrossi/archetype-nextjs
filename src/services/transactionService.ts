// src/services/transactionService.ts

import { Transaction, CreateTransactionData, ApiError } from '@/types/account';

const API_BASE_URL = '/api';

class TransactionService {
  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: ApiError = await response.json().catch(() => ({
        error: 'An unexpected error occurred',
        status: response.status,
      }));
      throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getTransactions(): Promise<Transaction[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction[]>(response);
    } catch (error) {
      console.error('Error fetching transactions:', error);
      throw error;
    }
  }

  async getTransactionById(transactionId: string): Promise<Transaction> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/${transactionId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction>(response);
    } catch (error) {
      console.error(`Error fetching transaction ${transactionId}:`, error);
      throw error;
    }
  }

  async getTransactionsByCardNumber(cardNumber: string): Promise<Transaction[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/card/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction[]>(response);
    } catch (error) {
      console.error(`Error fetching transactions by card ${cardNumber}:`, error);
      throw error;
    }
  }

  async getTransactionsByAccountId(accountId: string): Promise<Transaction[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/account/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction[]>(response);
    } catch (error) {
      console.error(`Error fetching transactions by account ${accountId}:`, error);
      throw error;
    }
  }

  async createTransaction(transactionData: CreateTransactionData): Promise<Transaction> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(transactionData),
      });
      return this.handleResponse<Transaction>(response);
    } catch (error) {
      console.error('Error creating transaction:', error);
      throw error;
    }
  }
}

export const transactionService = new TransactionService();
export default TransactionService;
