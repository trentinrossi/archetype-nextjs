import { CardCrossReference, CreateCardCrossReferenceRequest, UpdateCardCrossReferenceRequest } from '@/types/card-cross-reference';

const API_BASE_URL = '/api';

class CardCrossReferenceService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCardCrossReferences(page: number = 0, size: number = 20, sort?: string): Promise<{ content: CardCrossReference[]; totalElements: number; totalPages: number }> {
    let url = `${API_BASE_URL}/card-cross-references?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card cross references');
    }
    return response.json();
  }

  async getCardCrossReferenceByAccountId(accountId: string | number): Promise<CardCrossReference> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Card cross reference not found');
      }
      throw new Error('Failed to fetch card cross reference');
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
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to create card cross reference');
    }
    return response.json();
  }

  async updateCardCrossReference(accountId: string | number, data: UpdateCardCrossReferenceRequest): Promise<CardCrossReference> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/${accountId}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to update card cross reference');
    }
    return response.json();
  }

  async deleteCardCrossReference(accountId: string | number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/card-cross-references/${accountId}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete card cross reference');
    }
  }
}

export const cardCrossReferenceService = new CardCrossReferenceService();
