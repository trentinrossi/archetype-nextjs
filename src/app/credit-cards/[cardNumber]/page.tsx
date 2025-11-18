'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard } from '@/types/creditCard';
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
      const data = await creditCardService.getCreditCardByCardNumber(cardNumber);
      setCreditCard(data);
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to load credit card');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!creditCard) return;
    if (!confirm('Are you sure you want to delete this credit card?')) return;

    try {
      await creditCardService.deleteCreditCard(creditCard.cardNumber);
      router.push('/credit-cards');
    } catch (err: any) {
      alert(err.message || 'Failed to delete credit card');
      console.error(err);
    }
  };

  if (loading) {
    return <div className="p-6">Loading credit card details...</div>;
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
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
      <div className="p-6">
        <div className="text-gray-600 mb-4">Credit card not found</div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Credit Card Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/credit-cards/${creditCard.cardNumber}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
            Back to List
          </Button>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Card Number
            </label>
            <p className="text-gray-900">{creditCard.maskedCardNumber}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account Number
            </label>
            <p className="text-gray-900">{creditCard.accountId}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Embossed Name
            </label>
            <p className="text-gray-900">{creditCard.embossedName}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Card Status
            </label>
            <span
              className={`inline-block px-3 py-1 rounded text-sm font-semibold ${
                creditCard.isActive
                  ? 'bg-green-100 text-green-800'
                  : 'bg-gray-100 text-gray-800'
              }`}
            >
              {creditCard.isActive ? 'Active' : 'Inactive'}
            </span>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Expiration Date
            </label>
            <p className="text-gray-900">{creditCard.expirationDateFormatted}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Expiration Status
            </label>
            <span
              className={`inline-block px-3 py-1 rounded text-sm font-semibold ${
                creditCard.isExpired
                  ? 'bg-red-100 text-red-800'
                  : 'bg-green-100 text-green-800'
              }`}
            >
              {creditCard.isExpired ? 'Expired' : 'Valid'}
            </span>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              CVV Code
            </label>
            <p className="text-gray-900">{creditCard.cvvCode}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Version
            </label>
            <p className="text-gray-900">{creditCard.version}</p>
          </div>

          {creditCard.lastModifiedBy && (
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Last Modified By
              </label>
              <p className="text-gray-900">{creditCard.lastModifiedBy}</p>
            </div>
          )}

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Created At
            </label>
            <p className="text-gray-900">
              {new Date(creditCard.createdAt).toLocaleString()}
            </p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Updated At
            </label>
            <p className="text-gray-900">
              {new Date(creditCard.updatedAt).toLocaleString()}
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
