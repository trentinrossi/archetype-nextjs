import { 
  Account, 
  CreateAccountRequest, 
  UpdateAccountRequest, 
  AccountBalance, 
  ProcessPaymentRequest, 
  ProcessPaymentResponse,
  PaginatedAccounts 
} from '@/types/account';

const API_BASE_URL = '/api';

class AccountService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccounts(page: number = 0, size: number = 20, sort?: string): Promise<PaginatedAccounts> {
    let url = `${API_BASE_URL}/accounts?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch accounts');
    }
    return response.json();
  }

  async getAccountById(accountId: string): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch account');
    }
    return response.json();
  }

  async getAccountBalance(accountId: string): Promise<AccountBalance> {
    const response = await fetch(`${API_BASE_URL}/accounts/${accountId}/balance`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch account balance');
    }
    return response.json();
  }

  async createAccount(data: CreateAccountRequest): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to create account');
    }
    return response.json();
  }

  async updateAccount(accountId: string, data: UpdateAccountRequest): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to update account');
    }
    return response.json();
  }

  async deleteAccount(accountId: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete account');
    }
  }

  async processPayment(data: ProcessPaymentRequest): Promise<ProcessPaymentResponse> {
    const response = await fetch(`${API_BASE_URL}/accounts/process-payment`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to process payment');
    }
    return response.json();
  }
}

export const accountService = new AccountService();
