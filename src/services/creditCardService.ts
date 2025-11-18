import { 
  CreditCard, 
  CreateCreditCardRequest, 
  UpdateCreditCardRequest,
  CreditCardSearchParams,
  CreditCardDetailsParams
} from '@/types/creditCard';

const API_BASE_URL = '/api';

class CreditCardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    const userId = localStorage.getItem('user_id');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
      ...(userId && { 'X-User-ID': userId }),
    };
  }

  async getCreditCards(params?: CreditCardSearchParams): Promise<{ content: CreditCard[]; totalElements: number; totalPages: number }> {
    const queryParams = new URLSearchParams();
    if (params?.accountId) queryParams.append('accountId', params.accountId);
    if (params?.cardNumber) queryParams.append('cardNumber', params.cardNumber);
    if (params?.page !== undefined) queryParams.append('page', params.page.toString());
    if (params?.size !== undefined) queryParams.append('size', params.size.toString());
    if (params?.sort) queryParams.append('sort', params.sort);

    const queryString = queryParams.toString();
    const url = queryString ? `${API_BASE_URL}/credit-cards?${queryString}` : `${API_BASE_URL}/credit-cards`;

    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch credit cards');
    }
    return response.json();
  }

  async getCreditCardByCardNumber(cardNumber: string): Promise<CreditCard> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch credit card');
    }
    return response.json();
  }

  async getCreditCardsByAccountId(accountId: string): Promise<CreditCard[]> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch credit cards');
    }
    return response.json();
  }

  async searchCreditCards(params: CreditCardSearchParams): Promise<{ content: CreditCard[]; totalElements: number; totalPages: number }> {
    const queryParams = new URLSearchParams();
    if (params.accountId) queryParams.append('accountId', params.accountId);
    if (params.cardNumber) queryParams.append('cardNumber', params.cardNumber);
    if (params.page !== undefined) queryParams.append('page', params.page.toString());
    if (params.size !== undefined) queryParams.append('size', params.size.toString());
    if (params.sort) queryParams.append('sort', params.sort);

    const response = await fetch(`${API_BASE_URL}/credit-cards/search?${queryParams.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to search credit cards');
    }
    return response.json();
  }

  async getCreditCardDetails(params: CreditCardDetailsParams): Promise<CreditCard> {
    const queryParams = new URLSearchParams({
      accountId: params.accountId,
      cardNumber: params.cardNumber,
    });

    const response = await fetch(`${API_BASE_URL}/credit-cards/details?${queryParams.toString()}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to fetch credit card details');
    }
    return response.json();
  }

  async createCreditCard(data: CreateCreditCardRequest): Promise<CreditCard> {
    const response = await fetch(`${API_BASE_URL}/credit-cards`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to create credit card');
    }
    return response.json();
  }

  async updateCreditCard(cardNumber: string, data: UpdateCreditCardRequest): Promise<CreditCard> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      if (response.status === 409) {
        throw new Error('Concurrent update detected - card was modified by another user');
      }
      throw new Error(errorData.error || 'Failed to update credit card');
    }
    return response.json();
  }

  async deleteCreditCard(cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to delete credit card');
    }
  }

  async returnFromList(params?: CreditCardSearchParams): Promise<{ content: CreditCard[]; totalElements: number; totalPages: number }> {
    const queryParams = new URLSearchParams();
    if (params?.accountId) queryParams.append('accountId', params.accountId);
    if (params?.cardNumber) queryParams.append('cardNumber', params.cardNumber);
    if (params?.page !== undefined) queryParams.append('page', params.page.toString());
    if (params?.size !== undefined) queryParams.append('size', params.size.toString());

    const queryString = queryParams.toString();
    const url = queryString 
      ? `${API_BASE_URL}/credit-cards/return-from-list?${queryString}` 
      : `${API_BASE_URL}/credit-cards/return-from-list`;

    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to return from list');
    }
    return response.json();
  }
}

export const creditCardService = new CreditCardService();
