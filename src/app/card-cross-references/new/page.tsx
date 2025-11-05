'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { cardCrossReferenceService } from '@/services/cardCrossReferenceService';

export default function NewCardCrossReferencePage() {
  const router = useRouter();
  const [formData, setFormData] = useState({
    cardNumber: '',
    accountId: '',
    customerId: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!/^\d{16}$/.test(formData.cardNumber)) {
      setError('Card number must be exactly 16 digits');
      return;
    }
    if (!/^\d+$/.test(formData.accountId)) {
      setError('Account ID must be numeric');
      return;
    }
    if (!/^\d+$/.test(formData.customerId)) {
      setError('Customer ID must be numeric');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      await cardCrossReferenceService.createCardCrossReference({
        cardNumber: formData.cardNumber,
        accountId: parseInt(formData.accountId),
        customerId: parseInt(formData.customerId),
      });
      alert('Card cross reference created successfully!');
      router.push('/card-cross-references');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to create cross reference');
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
        <h1 className="text-2xl font-bold mb-6">Create Card Cross Reference</h1>

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
            label="Account ID"
            placeholder="Enter account ID"
            value={formData.accountId}
            onChange={(e) => setFormData({ ...formData, accountId: e.target.value })}
            required
          />

          <Input
            label="Customer ID"
            placeholder="Enter customer ID"
            value={formData.customerId}
            onChange={(e) => setFormData({ ...formData, customerId: e.target.value })}
            required
          />

          <div className="flex gap-4">
            <Button type="submit" disabled={loading}>
              {loading ? 'Creating...' : 'Create Cross Reference'}
            </Button>
            <Button type="button" variant="secondary" onClick={() => router.push('/card-cross-references')}>
              Cancel
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
}
