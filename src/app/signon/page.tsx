'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { UserSignonForm } from '@/components/UserSignonForm';

export default function SignonPage() {
  const router = useRouter();

  // COSGN00C business rule: Handle successful signon
  const handleSignonSuccess = (userType: 'ADMIN' | 'GENERAL', redirectProgram: string) => {
    // Store user type in session storage for navigation
    sessionStorage.setItem('userType', userType);
    sessionStorage.setItem('redirectProgram', redirectProgram);
    
    // Route based on user type and redirect program (COSGN00C business logic)
    if (userType === 'ADMIN' && redirectProgram === 'COADM01C') {
      router.push('/admin');
    } else if (userType === 'GENERAL' && redirectProgram === 'COMEN01C') {
      router.push('/general');
    } else {
      // Default fallback
      router.push(userType === 'ADMIN' ? '/admin' : '/general');
    }
  };

  // Handle signon exit (PF3)
  const handleSignonExit = () => {
    // In a real application, this might close the browser or redirect to a landing page
    console.log('Application exit requested');
    // Could redirect to a goodbye page or close the application
  };

  return (
    <UserSignonForm
      onSignonSuccess={handleSignonSuccess}
      onExit={handleSignonExit}
    />
  );
}