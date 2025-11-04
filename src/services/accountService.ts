import { 
  Account, 
  CreateAccountRequest, 
  UpdateAccountRequest
} from '@/types/cardDemo';

const API_BASE_URL = '/api';

class AccountService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccounts(): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts`, {
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

  async getAccountsByStatus(activeStatus: string): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts/status/${activeStatus}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch accounts by status');
    }
    return response.json();
  }

  async getAccountsByGroup(groupId: string): Promise<Account[]> {
    const response = await fetch(`${API_BASE_URL}/accounts/group/${groupId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch accounts by group');
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
      throw new Error('Failed to create account');
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
      throw new Error('Failed to update account');
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
}

export const accountService = new AccountService();
