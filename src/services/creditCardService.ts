import {
  CreditCard,
  CreditCardFilterRequest,
  CreditCardListResponse,
  User,
} from '@/types/credit-card';

const API_BASE_URL = '/api';

class CreditCardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCreditCards(
    page: number = 0,
    size: number = 20
  ): Promise<CreditCardListResponse> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards?page=${page}&size=${size}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to fetch credit cards');
    }
    return response.json();
  }

  async getCreditCardByNumber(cardNumber: string): Promise<CreditCard> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/${cardNumber}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to fetch credit card');
    }
    return response.json();
  }

  async filterCreditCards(
    userId: string,
    filterRequest: CreditCardFilterRequest
  ): Promise<CreditCardListResponse> {
    const page = filterRequest.page || 0;
    const size = filterRequest.size || 20;
    
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/filter?userId=${userId}&page=${page}&size=${size}`,
      {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(filterRequest),
      }
    );
    
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to filter credit cards');
    }
    
    return response.json();
  }

  async updateCreditCard(
    cardNumber: string,
    data: Partial<CreditCard>
  ): Promise<CreditCard> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/${cardNumber}`,
      {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to update credit card');
    }
    return response.json();
  }

  async deleteCreditCard(cardNumber: string): Promise<void> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/${cardNumber}`,
      {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to delete credit card');
    }
  }

  async validateSelection(selectionCount: number): Promise<void> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/validate-selection?selectionCount=${selectionCount}`,
      {
        method: 'POST',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Invalid selection');
    }
  }

  async validateBackwardNavigation(page: number): Promise<void> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/validate-backward-navigation?page=${page}`,
      {
        method: 'POST',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Cannot navigate backward');
    }
  }

  async validateForwardNavigation(pageInfo: {
    last: boolean;
  }): Promise<void> {
    const response = await fetch(
      `${API_BASE_URL}/credit-cards/validate-forward-navigation`,
      {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(pageInfo),
      }
    );
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Cannot navigate forward');
    }
  }

  async getUserById(userId: string): Promise<User> {
    const response = await fetch(`${API_BASE_URL}/users/${userId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user');
    }
    return response.json();
  }

  async getAccessibleAccounts(userId: string): Promise<string[]> {
    const response = await fetch(
      `${API_BASE_URL}/users/${userId}/accessible-accounts`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to fetch accessible accounts');
    }
    return response.json();
  }

  validateAccountFilter(accountId: string): {
    isValid: boolean;
    error?: string;
  } {
    if (!accountId || accountId.trim() === '') {
      return { isValid: true };
    }

    const trimmed = accountId.trim();
    
    if (trimmed === '00000000000' || /^\s+$/.test(accountId)) {
      return {
        isValid: false,
        error: 'ACCOUNT FILTER CANNOT BE BLANK, SPACES OR ZEROS',
      };
    }

    if (!/^\d{11}$/.test(trimmed)) {
      return {
        isValid: false,
        error: 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER',
      };
    }

    return { isValid: true };
  }

  validateCardNumberFilter(cardNumber: string): {
    isValid: boolean;
    error?: string;
  } {
    if (!cardNumber || cardNumber.trim() === '') {
      return { isValid: true };
    }

    const trimmed = cardNumber.trim();
    
    if (trimmed === '0000000000000000' || /^\s+$/.test(cardNumber)) {
      return {
        isValid: false,
        error: 'CARD FILTER CANNOT BE BLANK, SPACES OR ZEROS',
      };
    }

    if (!/^\d{16}$/.test(trimmed)) {
      return {
        isValid: false,
        error: 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER',
      };
    }

    return { isValid: true };
  }
}

export const creditCardService = new CreditCardService();
