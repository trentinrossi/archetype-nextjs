import {
  AccountValidationRequest,
  AccountValidationResponse,
  BalanceCheckRequest,
  BalanceCheckResponse,
  ProcessPaymentRequest,
  ProcessPaymentResponse,
} from '@/types/billPayment';

const API_BASE_URL = '/api';

class BillPaymentService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async validateAccount(data: AccountValidationRequest): Promise<AccountValidationResponse> {
    const response = await fetch(`${API_BASE_URL}/bill-payment/validate-account`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    const result = await response.json();

    if (!response.ok) {
      throw new Error(result.message || 'Failed to validate account');
    }

    return result;
  }

  async checkBalance(data: BalanceCheckRequest): Promise<BalanceCheckResponse> {
    const response = await fetch(`${API_BASE_URL}/bill-payment/check-balance`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    const result = await response.json();

    if (!response.ok) {
      throw new Error(result.message || 'Failed to check balance');
    }

    return result;
  }

  async processPayment(data: ProcessPaymentRequest): Promise<ProcessPaymentResponse> {
    const response = await fetch(`${API_BASE_URL}/bill-payment/process-payment`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });

    const result = await response.json();

    if (!response.ok) {
      throw new Error(result.message || 'Failed to process payment');
    }

    return result;
  }
}

export const billPaymentService = new BillPaymentService();
