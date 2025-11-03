// src/services/statementService.ts

import { AccountStatement, InterestCalculation, ApiError } from '@/types/account';

const API_BASE_URL = '/api';

class StatementService {
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
      throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getAllStatements(): Promise<AccountStatement[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/statements`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<AccountStatement[]>(response);
    } catch (error) {
      console.error('Error fetching statements:', error);
      throw error;
    }
  }

  async getStatementByCardNumber(cardNumber: string): Promise<AccountStatement> {
    try {
      const response = await fetch(`${API_BASE_URL}/statements/card/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<AccountStatement>(response);
    } catch (error) {
      console.error(`Error fetching statement for card ${cardNumber}:`, error);
      throw error;
    }
  }

  async getStatementsByAccountId(accountId: string): Promise<AccountStatement[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/statements/account/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<AccountStatement[]>(response);
    } catch (error) {
      console.error(`Error fetching statements for account ${accountId}:`, error);
      throw error;
    }
  }

  async calculateInterest(processingDate?: string): Promise<InterestCalculation[]> {
    try {
      const url = processingDate 
        ? `${API_BASE_URL}/interest/calculate?processingDate=${processingDate}`
        : `${API_BASE_URL}/interest/calculate`;
      
      const response = await fetch(url, {
        method: 'POST',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<InterestCalculation[]>(response);
    } catch (error) {
      console.error('Error calculating interest:', error);
      throw error;
    }
  }
}

export const statementService = new StatementService();
export default StatementService;
