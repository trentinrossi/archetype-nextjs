'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardCrossReferenceService } from '@/services/cardCrossReferenceService';
import { CreateCardCrossReferenceRequest } from '@/types/card-cross-reference';
import { Input, Button } from '@/components/ui';

export default function CreateCardCrossReferencePage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateCardCrossReferenceRequest>({
    accountId: '',
    cardNumber: '',
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (formData.accountId.length > 11) {
      alert('Account ID must be 11 characters or less');
      return;
    }
    
    if (formData.cardNumber.length !== 16) {
      alert('Card number must be exactly 16 digits');
      return;
    }
    
    try {
      setLoading(true);
      await cardCrossReferenceService.createCardCrossReference(formData);
      router.push('/card-cross-references');
    } catch (err: any) {
      alert(err.message || 'Failed to create card cross-reference');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create Card Cross Reference</h1>
      
      <div className="bg-blue-50 border border-blue-200 rounded p-4 mb-6">
        <p className="text-sm text-blue-800">
          Link a card number to an account. This allows the card to be used for transactions on the account.
        </p>
      </div>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Account ID"
          value={formData.accountId}
          onChange={(e) => setFormData({ ...formData, accountId: e.target.value })}
          maxLength={11}
          required
          placeholder="Enter account ID (max 11 characters)"
        />
        
        <Input
          label="Card Number"
          value={formData.cardNumber}
          onChange={(e) => {
            const value = e.target.value.replace(/\D/g, '');
            setFormData({ ...formData, cardNumber: value });
          }}
          maxLength={16}
          required
          placeholder="Enter 16-digit card number"
        />
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Link'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/card-cross-references')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
