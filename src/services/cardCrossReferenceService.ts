import { 
  CardCrossReference, 
  CreateCardCrossReferenceRequest,
  PaginatedCardCrossReferences 
} from '@/types/card-cross-reference';

const API_BASE_URL = '/api';

class CardCrossReferenceService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCardCrossReferences(page: number = 0, size: number = 20, sort?: string): Promise<PaginatedCardCrossReferences> {
    let url = `${API_BASE_URL}/card-cross-references?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card cross-references');
    }
    return response.json();
  }

  async getCardCrossReference(accountId: string, cardNumber: string): Promise<CardCrossReference> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/${accountId}/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card cross-reference');
    }
    return response.json();
  }

  async getCardCrossReferencesByAccount(accountId: string): Promise<CardCrossReference[]> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card cross-references by account');
    }
    return response.json();
  }

  async getCardCrossReferencesByCard(cardNumber: string): Promise<CardCrossReference[]> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/card/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card cross-references by card');
    }
    return response.json();
  }

  async createCardCrossReference(data: CreateCardCrossReferenceRequest): Promise<CardCrossReference> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Failed to create card cross-reference');
    }
    return response.json();
  }

  async deleteCardCrossReference(accountId: string, cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/${accountId}/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete card cross-reference');
    }
  }

  async deleteCardCrossReferencesByAccount(accountId: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/account/${accountId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete card cross-references by account');
    }
  }

  async deleteCardCrossReferencesByCard(cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/card/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete card cross-references by card');
    }
  }
}

export const cardCrossReferenceService = new CardCrossReferenceService();
