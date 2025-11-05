import { Account, CreateAccountRequest, UpdateAccountRequest, ApiError } from '@/types/cardServices';

const API_BASE_URL = '/api';

class AccountService {
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
      throw new Error(errorData.error || errorData.message || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getAccounts(): Promise<Account[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Account[]>(response);
    } catch (error) {
      console.error('Error fetching accounts:', error);
      throw error;
    }
  }

  async getAccountById(accountId: string): Promise<Account> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Account>(response);
    } catch (error) {
      console.error(`Error fetching account ${accountId}:`, error);
      throw error;
    }
  }

  async getAccountsByStatus(status: 'Y' | 'N'): Promise<Account[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts/status/${status}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Account[]>(response);
    } catch (error) {
      console.error(`Error fetching accounts by status ${status}:`, error);
      throw error;
    }
  }

  async createAccount(data: CreateAccountRequest): Promise<Account> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Account>(response);
    } catch (error) {
      console.error('Error creating account:', error);
      throw error;
    }
  }

  async updateAccount(accountId: string, data: UpdateAccountRequest): Promise<Account> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Account>(response);
    } catch (error) {
      console.error(`Error updating account ${accountId}:`, error);
      throw error;
    }
  }

  async deleteAccount(accountId: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/accounts/${accountId}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || errorData.message || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting account ${accountId}:`, error);
      throw error;
    }
  }
}

export const accountService = new AccountService();
export default AccountService;
