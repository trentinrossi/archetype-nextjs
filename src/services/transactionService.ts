import { Transaction, PaginatedTransactionsResponse, TransactionFilterParams } from '@/types/user';

const API_BASE_URL = '/api';

interface DateRangeParams {
  startDate: string;
  endDate: string;
}

interface UserTransactionsParams {
  userId: string;
  page?: number;
  size?: number;
}

class TransactionService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getTransactions(params?: TransactionFilterParams): Promise<PaginatedTransactionsResponse> {
    const queryParams = new URLSearchParams();
    
    if (params?.page !== undefined) {
      queryParams.append('page', params.page.toString());
    }
    if (params?.size !== undefined) {
      queryParams.append('size', params.size.toString());
    }
    if (params?.sort) {
      queryParams.append('sort', params.sort);
    }
    if (params?.userId) {
      queryParams.append('userId', params.userId);
    }
    if (params?.startDate) {
      queryParams.append('startDate', params.startDate);
    }
    if (params?.endDate) {
      queryParams.append('endDate', params.endDate);
    }
    if (params?.minAmount !== undefined) {
      queryParams.append('minAmount', params.minAmount.toString());
    }
    if (params?.maxAmount !== undefined) {
      queryParams.append('maxAmount', params.maxAmount.toString());
    }

    const url = `${API_BASE_URL}/transactions${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch transactions');
    }

    return response.json();
  }

  async getTransactionById(transactionId: string): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/${transactionId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (response.status === 400) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Invalid transaction ID');
    }

    if (response.status === 404) {
      throw new Error('Transaction not found');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch transaction');
    }

    return response.json();
  }

  async getTransactionsByDateRange(startDate: string, endDate: string): Promise<Transaction[]> {
    const queryParams = new URLSearchParams();
    queryParams.append('startDate', startDate);
    queryParams.append('endDate', endDate);

    const response = await fetch(`${API_BASE_URL}/transactions/date-range?${queryParams.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (response.status === 400) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Invalid date range format. Use yyyy-MM-dd');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch transactions by date range');
    }

    return response.json();
  }

  async getMonthlyTransactions(): Promise<{ transactions: Transaction[]; message: string }> {
    const response = await fetch(`${API_BASE_URL}/transactions/monthly`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch monthly transactions');
    }

    return response.json();
  }

  async getYearlyTransactions(): Promise<{ transactions: Transaction[]; message: string }> {
    const response = await fetch(`${API_BASE_URL}/transactions/yearly`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch yearly transactions');
    }

    return response.json();
  }

  async getUserTransactions(userId: string, page?: number, size?: number): Promise<PaginatedTransactionsResponse> {
    const queryParams = new URLSearchParams();
    
    if (page !== undefined) {
      queryParams.append('page', page.toString());
    }
    if (size !== undefined) {
      queryParams.append('size', size.toString());
    }

    const url = `${API_BASE_URL}/transactions/user/${userId}${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (response.status === 404) {
      throw new Error('User not found');
    }

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch user transactions');
    }

    return response.json();
  }
}

export const transactionService = new TransactionService();
