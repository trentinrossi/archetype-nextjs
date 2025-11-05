'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { cardService } from '@/services/cardService';

export default function NewCardPage() {
  const router = useRouter();
  const [formData, setFormData] = useState({
    cardNumber: '',
    status: 'ACTIVE',
    cardDetails: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!/^\d{16}$/.test(formData.cardNumber)) {
      setError('Card number must be exactly 16 digits');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      await cardService.createCard(formData);
      alert('Card created successfully!');
      router.push('/cards');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to create card');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-3xl mx-auto">
      <button onClick={() => router.back()} className="text-blue-600 hover:text-blue-800 flex items-center gap-2 mb-4">
        <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
        </svg>
        Back
      </button>

      <div className="bg-white shadow rounded-lg p-6">
        <h1 className="text-2xl font-bold mb-6">Create New Card</h1>

        {error && (
          <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-sm text-red-700">{error}</p>
          </div>
        )}

        <form onSubmit={handleSubmit} className="space-y-6">
          <Input
            label="Card Number"
            placeholder="Enter 16-digit card number"
            value={formData.cardNumber}
            onChange={(e) => setFormData({ ...formData, cardNumber: e.target.value })}
            required
          />

          <Input
            label="Status"
            placeholder="Enter card status"
            value={formData.status}
            onChange={(e) => setFormData({ ...formData, status: e.target.value })}
            required
          />

          <Input
            label="Card Details"
            placeholder="Enter card details (optional)"
            value={formData.cardDetails}
            onChange={(e) => setFormData({ ...formData, cardDetails: e.target.value })}
          />

          <div className="flex gap-4">
            <Button type="submit" disabled={loading}>
              {loading ? 'Creating...' : 'Create Card'}
            </Button>
            <Button type="button" variant="secondary" onClick={() => router.push('/cards')}>
              Cancel
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
}
