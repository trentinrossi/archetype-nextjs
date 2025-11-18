import { Account, CreateAccountRequest, AccountSearchParams } from '@/types/account';

const API_BASE_URL = '/api';

class AccountService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccounts(params?: AccountSearchParams): Promise<{ content: Account[]; totalElements: number; totalPages: number }> {
    const queryParams = new URLSearchParams();
    if (params?.searchTerm) queryParams.append('searchTerm', params.searchTerm);
    if (params?.page !== undefined) queryParams.append('page', params.page.toString());
    if (params?.size !== undefined) queryParams.append('size', params.size.toString());
    if (params?.sort) queryParams.append('sort', params.sort);

    const queryString = queryParams.toString();
    const url = queryString ? `${API_BASE_URL}/accounts?${queryString}` : `${API_BASE_URL}/accounts`;

    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch accounts');
    }
    return response.json();
  }

  async getAccountById(id: string): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch account');
    }
    return response.json();
  }

  async searchAccounts(searchTerm: string, params?: AccountSearchParams): Promise<{ content: Account[]; totalElements: number; totalPages: number }> {
    const queryParams = new URLSearchParams({ searchTerm });
    if (params?.page !== undefined) queryParams.append('page', params.page.toString());
    if (params?.size !== undefined) queryParams.append('size', params.size.toString());
    if (params?.sort) queryParams.append('sort', params.sort);

    const response = await fetch(`${API_BASE_URL}/accounts/search?${queryParams.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to search accounts');
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

  async updateAccount(id: string, data: Partial<Account>): Promise<Account> {
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
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to delete account');
    }
  }
}

export const accountService = new AccountService();
