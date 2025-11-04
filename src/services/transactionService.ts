import { 
  Transaction, 
  TransactionListResponse, 
  DateRangeParams,
  TransactionReportResponse
} from '@/types/transaction';
import { authService } from './authService';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || '/api';

class TransactionService {
  private getAuthHeaders(): Record<string, string> {
    const token = authService.getAuthToken();
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getTransactions(page: number = 0, size: number = 10, startTransactionId?: string): Promise<TransactionListResponse> {
    const params = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
    });

    if (startTransactionId) {
      params.append('startTransactionId', startTransactionId);
    }

    const response = await fetch(`${API_BASE_URL}/transactions?${params.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Transactions not found');
      }
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Bad request');
      }
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
      if (response.status === 404) {
        throw new Error('Transaction not found');
      }
      throw new Error('Failed to fetch transaction');
    }

    return response.json();
  }

  async getTransactionsByDateRange(params: DateRangeParams): Promise<Transaction[]> {
    const queryParams = new URLSearchParams({
      startDate: params.startDate,
      endDate: params.endDate,
    });

    const response = await fetch(`${API_BASE_URL}/transactions/date-range?${queryParams.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('No transactions found for the specified date range');
      }
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Invalid date format. Expected: yyyy-MM-dd');
      }
      throw new Error('Failed to fetch transactions by date range');
    }

    return response.json();
  }

  async getMonthlyTransactions(): Promise<TransactionReportResponse> {
    const response = await fetch(`${API_BASE_URL}/transactions/monthly`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('No monthly transactions found');
      }
      throw new Error('Failed to fetch monthly transactions');
    }

    return response.json();
  }

  async getYearlyTransactions(): Promise<TransactionReportResponse> {
    const response = await fetch(`${API_BASE_URL}/transactions/yearly`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('No yearly transactions found');
      }
      throw new Error('Failed to fetch yearly transactions');
    }

    return response.json();
  }

  async getUserTransactions(userId: string, page: number = 0, size: number = 10): Promise<TransactionListResponse> {
    const params = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
    });

    const response = await fetch(`${API_BASE_URL}/transactions/user/${userId}?${params.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('User transactions not found');
      }
      if (response.status === 400) {
        const errorData = await response.json();
        throw new Error(errorData.message || 'Bad request');
      }
      throw new Error('Failed to fetch user transactions');
    }

    return response.json();
  }
}

export const transactionService = new TransactionService();
