import { 
  InterestCalculation, 
  AccountStatement, 
  CalculateInterestRequest,
  CalculateInterestResponse
} from '@/types/statement';

const API_BASE_URL = '/api';

class StatementService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async calculateInterest(processingDate: string): Promise<CalculateInterestResponse[]> {
    const response = await fetch(`${API_BASE_URL}/interest/calculate?processingDate=${encodeURIComponent(processingDate)}`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to calculate interest');
    }
    return response.json();
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

  async getStatementByCard(cardNumber: string): Promise<AccountStatement> {
    const response = await fetch(`${API_BASE_URL}/statements/card/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch statement by card');
    }
    return response.json();
  }

  async getStatementsByAccount(accountId: string): Promise<AccountStatement[]> {
    const response = await fetch(`${API_BASE_URL}/statements/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch statements by account');
    }
    return response.json();
  }
}

export const statementService = new StatementService();
