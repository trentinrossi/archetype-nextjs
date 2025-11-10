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

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this credit card?')) return;
    
    try {
      await creditCardService.deleteCreditCard(params.cardNumber as string);
      router.push('/credit-cards');
    } catch (err) {
      alert('Failed to delete credit card');
      console.error(err);
    }
  };

  const getStatusBadgeColor = (status: string) => {
    switch (status) {
      case 'A':
        return 'bg-green-100 text-green-800';
      case 'I':
        return 'bg-gray-100 text-gray-800';
      case 'B':
        return 'bg-red-100 text-red-800';
      case 'C':
        return 'bg-black text-white';
      case 'S':
        return 'bg-yellow-100 text-yellow-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const formatCardNumber = (cardNumber: string) => {
    // Format as XXXX-XXXX-XXXX-XXXX
    return cardNumber.replace(/(\d{4})(?=\d)/g, '$1-');
  };

  if (loading) {
    return (
      <div className="p-6">
        <div className="flex items-center justify-center h-64">
          <div className="text-lg">Loading credit card details...</div>
        </div>
      </div>
    );
  }

  if (error || !creditCard) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error || 'Credit card not found'}
        </div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      {/* Header */}
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Credit Card Details</h1>
          <p className="text-sm text-gray-600 mt-1">
            Card Number: {creditCard.maskedCardNumber || formatCardNumber(creditCard.cardNumber)}
          </p>
        </div>
        <div className="flex gap-2">
          {creditCard.canModify && (
            <Button onClick={() => router.push(`/credit-cards/${creditCard.cardNumber}/edit`)}>
              Edit Card
            </Button>
          )}
          <Button variant="danger" onClick={handleDelete}>
            Delete Card
          </Button>
          <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
            Back to List
          </Button>
        </div>
      </div>

      {/* Card Information */}
      <div className="bg-white shadow rounded-lg overflow-hidden">
        <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
          <h2 className="text-lg font-semibold">Card Information</h2>
        </div>
        
        <div className="p-6 space-y-6">
          {/* Basic Information */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Number
              </label>
              <p className="text-gray-900 font-mono">
                {creditCard.maskedCardNumber || formatCardNumber(creditCard.cardNumber)}
              </p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Account ID
              </label>
              <p className="text-gray-900">{creditCard.accountId}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Cardholder Name
              </label>
              <p className="text-gray-900">{creditCard.cardholderName || '-'}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Status
              </label>
              <div>
                <span className={`px-3 py-1 inline-flex text-sm leading-5 font-semibold rounded-full ${getStatusBadgeColor(creditCard.cardStatus)}`}>
                  {creditCard.cardStatusDisplayName}
                </span>
              </div>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Type
              </label>
              <p className="text-gray-900">{creditCard.cardType || '-'}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Expiry Date
              </label>
              <p className="text-gray-900">
                {creditCard.expiryMonth && creditCard.expiryYear 
                  ? `${creditCard.expiryMonth}/${creditCard.expiryYear}`
                  : '-'
                }
                {creditCard.isExpired && (
                  <span className="ml-2 text-red-600 text-sm font-semibold">(EXPIRED)</span>
                )}
              </p>
            </div>
          </div>

          {/* Financial Information */}
          <div className="border-t border-gray-200 pt-6">
            <h3 className="text-md font-semibold text-gray-900 mb-4">Financial Information</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">
                  Credit Limit
                </label>
                <p className="text-gray-900 text-lg">
                  {creditCard.creditLimit !== undefined
                    ? `$${creditCard.creditLimit.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`
                    : '-'
                  }
                </p>
              </div>
              
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">
                  Available Credit
                </label>
                <p className="text-gray-900 text-lg">
                  {creditCard.availableCredit !== undefined
                    ? `$${creditCard.availableCredit.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`
                    : '-'
                  }
                </p>
              </div>
            </div>
            
            {creditCard.creditLimit !== undefined && creditCard.availableCredit !== undefined && (
              <div className="mt-4">
                <label className="block text-sm font-semibold text-gray-700 mb-2">
                  Credit Utilization
                </label>
                <div className="w-full bg-gray-200 rounded-full h-4">
                  <div
                    className="bg-blue-600 h-4 rounded-full"
                    style={{
                      width: `${((creditCard.creditLimit - creditCard.availableCredit) / creditCard.creditLimit) * 100}%`
                    }}
                  ></div>
                </div>
                <p className="text-sm text-gray-600 mt-1">
                  {((creditCard.creditLimit - creditCard.availableCredit) / creditCard.creditLimit * 100).toFixed(1)}% utilized
                </p>
              </div>
            )}
          </div>

          {/* Status Indicators */}
          <div className="border-t border-gray-200 pt-6">
            <h3 className="text-md font-semibold text-gray-900 mb-4">Status Indicators</h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="flex items-center">
                <div className={`w-3 h-3 rounded-full mr-2 ${creditCard.isActive ? 'bg-green-500' : 'bg-gray-300'}`}></div>
                <span className="text-sm text-gray-700">
                  {creditCard.isActive ? 'Active' : 'Not Active'}
                </span>
              </div>
              
              <div className="flex items-center">
                <div className={`w-3 h-3 rounded-full mr-2 ${creditCard.isExpired ? 'bg-red-500' : 'bg-green-500'}`}></div>
                <span className="text-sm text-gray-700">
                  {creditCard.isExpired ? 'Expired' : 'Valid'}
                </span>
              </div>
              
              <div className="flex items-center">
                <div className={`w-3 h-3 rounded-full mr-2 ${creditCard.canModify ? 'bg-green-500' : 'bg-gray-300'}`}></div>
                <span className="text-sm text-gray-700">
                  {creditCard.canModify ? 'Can Modify' : 'Cannot Modify'}
                </span>
              </div>
            </div>
          </div>

          {/* Timestamps */}
          <div className="border-t border-gray-200 pt-6">
            <h3 className="text-md font-semibold text-gray-900 mb-4">Record Information</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
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
                  Last Updated
                </label>
                <p className="text-gray-900">
                  {new Date(creditCard.updatedAt).toLocaleString()}
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Action Buttons */}
      <div className="mt-6 flex gap-2">
        {creditCard.canModify && (
          <Button onClick={() => router.push(`/credit-cards/${creditCard.cardNumber}/edit`)}>
            Edit Card
          </Button>
        )}
        <Button variant="danger" onClick={handleDelete}>
          Delete Card
        </Button>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    </div>
  );
}
