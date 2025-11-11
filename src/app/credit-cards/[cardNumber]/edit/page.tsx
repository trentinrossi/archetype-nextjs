'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard, CARD_STATUS_OPTIONS, CardStatusCode } from '@/types/credit-card';
import { Input, Select, Button } from '@/components/ui';

export default function EditCreditCardPage() {
  const params = useParams();
  const router = useRouter();
  const [creditCard, setCreditCard] = useState<CreditCard | null>(null);
  const [cardStatus, setCardStatus] = useState<CardStatusCode>('A');
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.cardNumber) {
      fetchCreditCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  const fetchCreditCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await creditCardService.getCreditCardByNumber(cardNumber);
      setCreditCard(data);
      setCardStatus(data.cardStatus as CardStatusCode);
      setError(null);
    } catch (err) {
      setError('Failed to load credit card details');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!creditCard) return;
    
    try {
      setSaving(true);
      setError(null);
      
      await creditCardService.updateCreditCard(creditCard.cardNumber, {
        cardStatus,
      });
      
      // Navigate back to detail view
      router.push(`/credit-cards/${creditCard.cardNumber}`);
    } catch (err) {
      setError('Failed to update credit card');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    if (creditCard) {
      router.push(`/credit-cards/${creditCard.cardNumber}`);
    } else {
      router.push('/credit-cards');
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading credit card details...</div>
      </div>
    );
  }

  if (error && !creditCard) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  if (!creditCard) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          Credit card not found
        </div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h1 className="text-3xl font-bold text-gray-900">
            Update Credit Card
          </h1>
          <p className="text-sm text-gray-600 mt-1">
            Card Number: {creditCard.formattedCardNumber}
          </p>
        </div>

        {/* Error Message */}
        {error && (
          <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
            {error}
          </div>
        )}

        {/* Edit Form */}
        <form onSubmit={handleSubmit}>
          <div className="bg-white shadow rounded-lg p-6 mb-6">
            <h2 className="text-xl font-semibold mb-4">Card Information</h2>
            
            <div className="space-y-6">
              {/* Read-only fields */}
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">
                    Card Number
                  </label>
                  <Input
                    value={creditCard.formattedCardNumber}
                    disabled
                  />
                </div>

                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">
                    Account ID
                  </label>
                  <Input
                    value={creditCard.accountId}
                    disabled
                  />
                </div>
              </div>

              {/* Editable field */}
              <div>
                <Select
                  label="Card Status"
                  value={cardStatus}
                  onChange={(e) => setCardStatus(e.target.value as CardStatusCode)}
                  options={CARD_STATUS_OPTIONS}
                  required
                />
                <p className="text-sm text-gray-600 mt-1">
                  Current status: {creditCard.cardStatusDisplayName}
                </p>
              </div>

              {/* Status indicators */}
              <div className="border-t pt-4">
                <h3 className="text-sm font-semibold text-gray-700 mb-3">
                  Current Status Indicators
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div className="flex items-center gap-2">
                    <span
                      className={`w-3 h-3 rounded-full ${
                        creditCard.isActive ? 'bg-green-500' : 'bg-gray-300'
                      }`}
                    ></span>
                    <span className="text-sm text-gray-700">
                      {creditCard.isActive ? 'Active' : 'Inactive'}
                    </span>
                  </div>
                  <div className="flex items-center gap-2">
                    <span
                      className={`w-3 h-3 rounded-full ${
                        creditCard.isBlocked ? 'bg-red-500' : 'bg-gray-300'
                      }`}
                    ></span>
                    <span className="text-sm text-gray-700">
                      {creditCard.isBlocked ? 'Blocked' : 'Not Blocked'}
                    </span>
                  </div>
                  <div className="flex items-center gap-2">
                    <span
                      className={`w-3 h-3 rounded-full ${
                        creditCard.isClosed ? 'bg-gray-500' : 'bg-gray-300'
                      }`}
                    ></span>
                    <span className="text-sm text-gray-700">
                      {creditCard.isClosed ? 'Closed' : 'Open'}
                    </span>
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Action Buttons */}
          <div className="bg-white shadow rounded-lg p-6">
            <div className="flex gap-2">
              <Button type="submit" disabled={saving}>
                {saving ? 'Saving...' : 'Save Changes'}
              </Button>
              <Button
                type="button"
                variant="secondary"
                onClick={handleCancel}
                disabled={saving}
              >
                Cancel
              </Button>
            </div>
          </div>
        </form>

        {/* Help Text */}
        <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h3 className="text-sm font-semibold text-blue-900 mb-2">
            Card Status Codes
          </h3>
          <ul className="text-sm text-blue-800 space-y-1">
            <li><strong>A</strong> - Active: Card is active and can be used</li>
            <li><strong>I</strong> - Inactive: Card is inactive</li>
            <li><strong>B</strong> - Blocked: Card is blocked</li>
            <li><strong>P</strong> - Pending: Card is pending activation</li>
            <li><strong>C</strong> - Closed: Card is closed</li>
            <li><strong>S</strong> - Suspended: Card is suspended</li>
            <li><strong>E</strong> - Expired: Card has expired</li>
            <li><strong>L</strong> - Lost: Card is reported as lost</li>
            <li><strong>T</strong> - Stolen: Card is reported as stolen</li>
            <li><strong>D</strong> - Damaged: Card is damaged</li>
          </ul>
        </div>
      </div>
    </div>
  );
}
