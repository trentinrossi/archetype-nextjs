import {
  Account,
  CreateAccountRequest,
  UpdateAccountRequest,
  AccountsPageResponse,
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

  async getAccounts(
    page: number = 0,
    size: number = 20,
    sort?: string
  ): Promise<AccountsPageResponse> {
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

  async getAccountById(id: string): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch account');
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
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to create account');
    }

    return response.json();
  }

  async updateAccount(id: string, data: UpdateAccountRequest): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to update account');
    }

    return response.json();
  }

  async deleteAccount(id: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to delete account');
    }
  }

  async getAccountsSequentially(): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts/sequential`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch accounts sequentially');
    }

    return response.json();
  }

  async getActiveAccounts(): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts/active`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch active accounts');
    }

    return response.json();
  }

  async getExpiredAccounts(): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts/expired`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to fetch expired accounts');
    }

    return response.json();
  }

  async checkAccountExists(id: string): Promise<boolean> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}/exists`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });

    if (!response.ok) {
      throw new Error('Failed to check account existence');
    }

    return response.json();
  }
}

export const accountService = new AccountService();
