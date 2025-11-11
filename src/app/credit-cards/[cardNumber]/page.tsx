'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard } from '@/types/credit-card';
import { Button } from '@/components/ui';

export default function CreditCardDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [creditCard, setCreditCard] = useState<CreditCard | null>(null);
  const [loading, setLoading] = useState(true);
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
      setError(null);
    } catch (err) {
      setError('Failed to load credit card details');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleEdit = () => {
    router.push(`/credit-cards/${params.cardNumber}/edit`);
  };

  const handleBack = () => {
    router.push('/credit-cards');
  };

  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading credit card details...</div>
      </div>
    );
  }

  if (error || !creditCard) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error || 'Credit card not found'}
        </div>
        <Button variant="secondary" onClick={handleBack}>
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
          <div className="flex justify-between items-start">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">
                Credit Card Details
              </h1>
              <p className="text-sm text-gray-600 mt-1">
                Card Number: {creditCard.formattedCardNumber}
              </p>
            </div>
            <div className="flex gap-2">
              <Button onClick={handleEdit}>Edit Card</Button>
              <Button variant="secondary" onClick={handleBack}>
                Back to List
              </Button>
            </div>
          </div>
        </div>

        {/* Card Information */}
        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-xl font-semibold mb-4">Card Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Number
              </label>
              <p className="text-gray-900">{creditCard.formattedCardNumber}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Masked Card Number
              </label>
              <p className="text-gray-900">{creditCard.maskedCardNumber}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Account ID
              </label>
              <p className="text-gray-900">{creditCard.accountId}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Status
              </label>
              <div className="flex items-center gap-2">
                <span
                  className={`px-3 py-1 rounded text-sm font-semibold ${
                    creditCard.isActive
                      ? 'bg-green-100 text-green-800'
                      : creditCard.isBlocked
                      ? 'bg-red-100 text-red-800'
                      : creditCard.isClosed
                      ? 'bg-gray-100 text-gray-800'
                      : 'bg-yellow-100 text-yellow-800'
                  }`}
                >
                  {creditCard.cardStatusDisplayName}
                </span>
                <span className="text-gray-600 text-sm">
                  ({creditCard.cardStatus})
                </span>
              </div>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Active Status
              </label>
              <p className="text-gray-900">
                {creditCard.isActive ? 'Active' : 'Inactive'}
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Blocked Status
              </label>
              <p className="text-gray-900">
                {creditCard.isBlocked ? 'Blocked' : 'Not Blocked'}
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Closed Status
              </label>
              <p className="text-gray-900">
                {creditCard.isClosed ? 'Closed' : 'Open'}
              </p>
            </div>
          </div>
        </div>

        {/* Timestamps */}
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Audit Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Created At
              </label>
              <p className="text-gray-900">{formatDate(creditCard.createdAt)}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Last Updated
              </label>
              <p className="text-gray-900">{formatDate(creditCard.updatedAt)}</p>
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="mt-6 flex gap-2">
          <Button onClick={handleEdit}>Edit Card</Button>
          <Button variant="secondary" onClick={handleBack}>
            Back to List
          </Button>
        </div>
      </div>
    </div>
  );
}
