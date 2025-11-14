import { Account, UpdateAccountRequest, AccountWithCustomer } from '@/types/account';

const API_BASE_URL = '/api';

class AccountService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccountById(id: string): Promise<Account> {
    const response = await fetch(`${API_BASE_URL}/accounts/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Account not found');
      }
      throw new Error('Failed to fetch account');
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
      if (response.status === 404) {
        throw new Error('Account not found');
      }
      if (response.status === 409) {
        throw new Error('Concurrent update detected. Please refresh and try again.');
      }
      throw new Error('Failed to update account');
    }
    return response.json();
  }

  async getAccountWithCustomer(accountId: string): Promise<AccountWithCustomer> {
    try {
      const account = await this.getAccountById(accountId);
      const customer = await this.getCustomerById(account.customerId.toString());
      return { account, customer };
    } catch (error) {
      throw error;
    }
  }

  private async getCustomerById(id: string): Promise<any> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Customer not found');
      }
      throw new Error('Failed to fetch customer');
    }
    return response.json();
  }
}

export const accountService = new AccountService();
