import { 
  AccountBalance, 
  ProcessPaymentRequest, 
  ProcessPaymentResponse,
  Transaction 
} from '@/types/bill-payment';

const API_BASE_URL = '/api';

class BillPaymentService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getAccountBalance(accountId: string): Promise<AccountBalance> {
    const response = await fetch(`${API_BASE_URL}/bill-payment/balance/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch account balance');
    }
    return response.json();
  }

  async processPayment(data: ProcessPaymentRequest): Promise<ProcessPaymentResponse> {
    const response = await fetch(`${API_BASE_URL}/bill-payment`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to process payment');
    }
    return response.json();
  }

  async getBillPaymentTransactions(accountId: string): Promise<Transaction[]> {
    const response = await fetch(`${API_BASE_URL}/bill-payment/transactions/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.message || 'Failed to fetch bill payment transactions');
    }
    return response.json();
  }
}

export const billPaymentService = new BillPaymentService();
