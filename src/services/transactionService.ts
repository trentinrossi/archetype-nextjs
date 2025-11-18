import { 
  Transaction, 
  TransactionListPage, 
  CreateTransactionRequest, 
  UpdateTransactionRequest,
  TransactionSearchParams 
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

  async getTransactions(page: number = 0, size: number = 10, sort?: string): Promise<TransactionListPage> {
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

  async getTransactionById(id: string): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transaction');
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
      throw new Error(errorData.error || 'Failed to create transaction');
    }
    return response.json();
  }

  async updateTransaction(id: string, data: UpdateTransactionRequest): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update transaction');
    }
    return response.json();
  }

  async deleteTransaction(id: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/transactions/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete transaction');
    }
  }

  async getTransactionPage(pageNumber: number): Promise<TransactionListPage> {
    const response = await fetch(`${API_BASE_URL}/transactions/page/${pageNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch transaction page');
    }
    return response.json();
  }

  async searchTransactions(params: TransactionSearchParams): Promise<TransactionListPage> {
    const { startTransactionId, pageNumber = 1 } = params;
    const response = await fetch(
      `${API_BASE_URL}/transactions/search/${startTransactionId}?pageNumber=${pageNumber}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to search transactions');
    }
    return response.json();
  }

  async navigateForward(lastTransactionId: string): Promise<TransactionListPage> {
    const response = await fetch(
      `${API_BASE_URL}/transactions/navigate/forward/${lastTransactionId}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to navigate forward');
    }
    return response.json();
  }

  async navigateBackward(firstTransactionId: string, currentPage: number): Promise<TransactionListPage> {
    const response = await fetch(
      `${API_BASE_URL}/transactions/navigate/backward/${firstTransactionId}?currentPage=${currentPage}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to navigate backward');
    }
    return response.json();
  }

  async copyLastTransaction(): Promise<Transaction> {
    const response = await fetch(`${API_BASE_URL}/transactions/copy-last`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to copy last transaction');
    }
    return response.json();
  }
}

export const transactionService = new TransactionService();
