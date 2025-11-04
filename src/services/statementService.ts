import { AccountStatement } from '@/types/cardDemo';

const API_BASE_URL = '/api';

class StatementService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAllStatements(): Promise<AccountStatement[]> {
    const response = await fetch(`${API_BASE_URL}/statements`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch statements');
    }
    return response.json();
  }

  async getStatementByCardNumber(cardNumber: string): Promise<AccountStatement> {
    const response = await fetch(`${API_BASE_URL}/statements/card/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch statement by card number');
    }
    return response.json();
  }

  async getStatementsByAccountId(accountId: string): Promise<AccountStatement[]> {
    const response = await fetch(`${API_BASE_URL}/statements/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch statements by account ID');
    }
    return response.json();
  }
}

export const statementService = new StatementService();
