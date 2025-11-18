'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard } from '@/types/creditCard';
import { Input, Button } from '@/components/ui';

export default function CreditCardSearchPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState('');
  const [cardNumber, setCardNumber] = useState('');
  const [creditCard, setCreditCard] = useState<CreditCard | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<{ accountId?: string; cardNumber?: string }>({});

  const validateInputs = (): boolean => {
    const errors: { accountId?: string; cardNumber?: string } = {};

    if (!accountId && !cardNumber) {
      setError('Please provide at least one search criterion (Account ID or Card Number)');
      return false;
    }

    if (accountId && !/^\d{11}$/.test(accountId)) {
      errors.accountId = 'Account number must be exactly 11 digits';
    }

    if (cardNumber && !/^\d{16}$/.test(cardNumber)) {
      errors.cardNumber = 'Card number must be exactly 16 digits';
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateInputs()) {
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setCreditCard(null);

      const data = await creditCardService.getCreditCardDetails({
        accountId,
        cardNumber,
      });

      setCreditCard(data);
    } catch (err: any) {
      setError(err.message || 'Failed to find credit card');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleReset = () => {
    setAccountId('');
    setCardNumber('');
    setCreditCard(null);
    setError(null);
    setValidationErrors({});
  };

  const handleViewList = () => {
    router.push('/credit-cards');
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Credit Card Detail Search</h1>
        <div className="flex gap-2">
          <Button variant="secondary" onClick={handleViewList}>
            View Card List
          </Button>
          <Button variant="secondary" onClick={() => router.push('/')}>
            Main Menu
          </Button>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Search Criteria</h2>
        <form onSubmit={handleSearch} className="space-y-4">
          <div>
            <Input
              label="Account Number (11 digits)"
              value={accountId}
              onChange={(e) => {
                setAccountId(e.target.value);
                setValidationErrors({ ...validationErrors, accountId: undefined });
              }}
              placeholder="Enter 11-digit account number"
              maxLength={11}
            />
            {validationErrors.accountId && (
              <p className="text-red-600 text-sm mt-1">{validationErrors.accountId}</p>
            )}
          </div>

          <div>
            <Input
              label="Card Number (16 digits)"
              value={cardNumber}
              onChange={(e) => {
                setCardNumber(e.target.value);
                setValidationErrors({ ...validationErrors, cardNumber: undefined });
              }}
              placeholder="Enter 16-digit card number"
              maxLength={16}
            />
            {validationErrors.cardNumber && (
              <p className="text-red-600 text-sm mt-1">{validationErrors.cardNumber}</p>
            )}
          </div>

          <div className="flex gap-2 pt-4">
            <Button type="submit" disabled={loading}>
              {loading ? 'Searching...' : 'Search'}
            </Button>
            <Button type="button" variant="secondary" onClick={handleReset}>
              Reset
            </Button>
          </div>
        </form>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      {creditCard && (
        <div className="bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-lg font-semibold">Card Details</h2>
            <Button
              size="sm"
              onClick={() => router.push(`/credit-cards/${creditCard.cardNumber}/edit`)}
            >
              Update Card
            </Button>
          </div>

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
                Expiration Month
              </label>
              <p className="text-gray-900">{creditCard.expiryMonth}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Expiration Year
              </label>
              <p className="text-gray-900">{creditCard.expiryYear}</p>
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
          </div>
        </div>
      )}
    </div>
  );
}
