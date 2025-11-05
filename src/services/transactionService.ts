import { Transaction, CreateTransactionRequest, PaginatedTransactionsResponse, ApiError } from '@/types/transaction';

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

  async getTransactions(page: number = 0, size: number = 20, sort?: string): Promise<PaginatedTransactionsResponse> {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        size: size.toString(),
        ...(sort && { sort }),
      });
      const response = await fetch(`${API_BASE_URL}/transactions?${params}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<PaginatedTransactionsResponse>(response);
    } catch (error) {
      console.error('Error fetching transactions:', error);
      throw error;
    }
  }

  async getTransactionById(id: number): Promise<Transaction> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/${id}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction>(response);
    } catch (error) {
      console.error(`Error fetching transaction ${id}:`, error);
      throw error;
    }
  }

  async getTransactionByTransactionId(transactionId: string): Promise<Transaction> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/transaction-id/${transactionId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Transaction>(response);
    } catch (error) {
      console.error(`Error fetching transaction by transaction ID ${transactionId}:`, error);
      throw error;
    }
  }

  async getTransactionsByCardNumber(cardNumber: string, page: number = 0, size: number = 20): Promise<PaginatedTransactionsResponse> {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        size: size.toString(),
      });
      const response = await fetch(`${API_BASE_URL}/transactions/card/${cardNumber}?${params}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<PaginatedTransactionsResponse>(response);
    } catch (error) {
      console.error(`Error fetching transactions by card number ${cardNumber}:`, error);
      throw error;
    }
  }

  async getTransactionsByDateRange(startDate: string, endDate: string, page: number = 0, size: number = 20): Promise<PaginatedTransactionsResponse> {
    try {
      const params = new URLSearchParams({
        startDate,
        endDate,
        page: page.toString(),
        size: size.toString(),
      });
      const response = await fetch(`${API_BASE_URL}/transactions/date-range?${params}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<PaginatedTransactionsResponse>(response);
    } catch (error) {
      console.error(`Error fetching transactions by date range:`, error);
      throw error;
    }
  }

  async createTransaction(data: CreateTransactionRequest): Promise<Transaction> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Transaction>(response);
    } catch (error) {
      console.error('Error creating transaction:', error);
      throw error;
    }
  }

  async deleteTransaction(id: number): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/transactions/${id}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting transaction ${id}:`, error);
      throw error;
    }
  }
}

export const transactionService = new TransactionService();
export default TransactionService;
